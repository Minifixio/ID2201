-module(node1).
-export([start/1, start/2, start_debug/1, start_debug/2]).
-define(StabilizeTimeout, 1000).
-define(ConnectionTiemout, 1000).

node(Id, Predecessor, Successor, Debug) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Debug);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            if 
                Debug == true -> 
                    io:format("Node ~p: Notified. Predecessor: ~p, Successor: ~p~n", [Id, Pred, Successor]);
                true -> 
                    ok
            end,
            node(Id, Pred, Successor, Debug);
        {request, Peer} ->
            request(Peer, Predecessor),
            if 
                Debug == true -> 
                    io:format("Node ~p: Request sent to ~p~n", [Id, Peer]);
                true -> 
                    ok
            end,
            node(Id, Predecessor, Successor, Debug);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            if 
                Debug == true -> 
                    io:format("Node ~p: Stabilized. Predecessor: ~p, Successor: ~p~n", [Id, Predecessor, Succ]);
                true -> 
                    ok
            end,
            node(Id, Predecessor, Succ, Debug);
        stabilize ->
            if 
                Debug == true -> 
                    io:format("Node ~p: Stabilizing...~n", [Id]);
                true -> 
                    ok
            end,
            stabilize(Successor),
            node(Id, Predecessor, Successor, Debug);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Debug);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Debug);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Debug);
        debug ->
            io:format("Node ~p: Predecessor: ~p, Successor: ~p~n", [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor, Debug);
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
            node(Id, Predecessor, Successor, Debug)
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

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->  % No predecessor, so the notifying node becomes the predecessor
            {Nkey, Npid};
        {Pkey, _} -> 
            % Check if the notifying node should be the predecessor
            case key:between(Nkey, Pkey, Id) of
                true -> {Nkey, Npid};
                false -> Predecessor
            end
    end.


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
    spawn(fun() -> init(Id, Peer, true) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, false).
init(Id, Peer, Debug) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, Debug).


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