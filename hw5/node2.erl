-module(node2).
-export([start/1, start/2, start_debug/1, start_debug/2]).
-define(StabilizeTimeout, 500).
-define(ConnectionTiemout, 1000).

node(Id, Predecessor, Successor, Store, Debug) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Debug);
        {notify, New} ->
            {Pred, NewStore} = notify(New, Id, Predecessor, Store),
            if 
                Debug == true -> 
                    io:format("Node ~p: Notified. Predecessor: ~p, Successor: ~p~n", [Id, Pred, Successor]);
                true -> 
                    ok
            end,
            node(Id, Pred, Successor, NewStore, Debug);
        {request, Peer} ->
            request(Peer, Predecessor),
            if 
                Debug == true -> 
                    io:format("Node ~p: Request sent to ~p~n", [Id, Peer]);
                true -> 
                    ok
            end,
            node(Id, Predecessor, Successor, Store, Debug);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            if 
                Debug == true -> 
                    io:format("Node ~p: Stabilized. Predecessor: ~p, Successor: ~p~n", [Id, Predecessor, Succ]);
                true -> 
                    ok
            end,
            node(Id, Predecessor, Succ, Store, Debug);
        stabilize ->
            if 
                Debug == true -> 
                    io:format("Node ~p: Stabilizing...~n", [Id]);
                true -> 
                    ok
            end,
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Debug);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store, Debug);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, Debug);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store, Debug);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Debug);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Debug);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Debug);
        debug ->
            io:format("Node ~p: Predecessor: ~p, Successor: ~p, Store: ~p~n", [Id, Predecessor, Successor, Store]),
            node(Id, Predecessor, Successor, Store, Debug);
        terminate ->
            if 
                Debug == true -> 
                    io:format("Node ~p: Terminating...~n", [Id]);
                true -> 
                    ok
            end,
            ok;
        UnexpectedMsg ->
            if 
                Debug == true -> 
                    io:format("Node ~p: Unexpected message: ~p~n", [Id, UnexpectedMsg]);
                true -> 
                    ok
            end,
            node(Id, Predecessor, Successor, Store, Debug)
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->  
            % If the successor has no predecessor, notify the successor of our existence
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} -> 
            % If we are already the predecessor of our successor, no action is needed
            Successor;
        {Skey, _} -> % 
            % If the predecessor of the successor is the same as the successor itself, notify the successor of our existence
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} -> 
            % If the predecessor is another node
            case key:between(Xkey, Id, Skey) of
                true -> 
                    % The predecessor is closer to the successor than we are.
                    % We adopt it as our new successor and restart stabilization.
                    Xpid ! {request, self()},
                    {Xkey, Xpid};
                false -> 
                    % The successor is in the correct position, so notify it of our existence
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

schedule_stabilize() ->
    timer:send_interval(?StabilizeTimeout, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->  % No predecessor, so the notifying node becomes the predecessor
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} -> 
            % Check if the notifying node should be the predecessor
            case key:between(Nkey, Pkey, Id) of
                true -> 
                    NewStore = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Npid}, NewStore};
                false -> 
                    {Predecessor, Store}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

create_probe(Id, Successor) ->
    Time = erlang:system_time(micro_seconds),
    {_, Spid} = Successor,
    Spid ! {probe, Id, [Id], Time}.

remove_probe(T, Nodes) ->
    RingPassTime = erlang:system_time(micro_seconds) - T,
    io:format("Probe completed in ~p microseconds~n", [RingPassTime]),
    io:format("Nodes in ring: ~p~n", [Nodes]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
    {_, Spid} = Successor,
    Spid ! {probe, Ref, [Id|Nodes], T}.

start(Id) ->
    start(Id, nil).
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

start_debug(Id) ->
    start(Id, nil).
start_debug(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init_debug(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create(), false).
init_debug(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create(), true).


connect(Id, nil) ->
    {ok, {Id, self()}};
connect(_, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
            {Qref, Skey} ->
                {ok, {Skey, Peer}}
        after ?ConnectionTiemout ->
                io:format("Time out: no response~n",[])
    end.