-module(gms4).
-export([start/1, start/2]).
-define(timeout, 1000).
-define(arghh, 100).
-define(ack_timeout, 500).

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> 
        crash(Id),
        send_with_ack(Id, Msg, Node) 
    end, Nodes).

send_with_ack(Id, Msg, Node) ->
    Node ! Msg,
    receive
        ack ->
            ok;
        {nack, Msg} ->
            io:format("leader ~w: resending message ~p to ~p~n", [Id, Msg, Node]),
            send_with_ack(Id, Msg, Node);
        stop -> 
            ok
    after ?ack_timeout ->
        io:format("leader ~w: timeout waiting for ack from ~p~n", [Id, Node]),
        send_with_ack(Id, Msg, Node)
    end.

crash(Id) ->
    case rand:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _-> ok
    end.

leader(Id, Master, N, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            io:format("leader ~w: mcast ~p~n", [Id, Msg]),
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group);
        {join, Wrk, Peer} ->
            io:format("node ~w joining group ~w~n", [Peer, Wrk]),
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2);
        stop -> 
            ok
    end.

election(Id, Master, N, Last, Slaves, Group) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Id, Last, Rest),
            bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, N+1, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            io:format("node ~w joining group ~w~n", [Id, Wrk]),
            Leader ! {join, Wrk, Peer},
            Leader ! ack,
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, I, _} when I < N ->
            io:format("slave ~w: wrong message order ~w < ~w~n", [Id, I, N]),
            Leader ! nack,
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, I, Msg} ->
            Master ! Msg,
            Leader ! ack,
            slave(Id, Master, Leader, N+1, {msg, I, Msg}, Slaves, Group);
        {view, I, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            Leader ! ack,
            slave(Id, Master, Leader, I+1, {view, I, [Leader|Slaves2], Group2}, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group);
        stop ->
            ok
    end.

start(Id) ->
    Rnd = rand:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.
init(Id, Rnd, Master) ->
    rand:seed(exs1024s, [Rnd, Rnd, Rnd]),
    leader(Id, Master, 0, [], [Master]).

start(Id, Grp) ->
    Rnd = rand:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Rnd, Self) end)}.
init(Id, Grp, Rnd, Master) ->
    rand:seed(exs1024s, [Rnd, Rnd, Rnd]),
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader|Slaves], Group} ->
            Leader ! ack,
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, {view, N, [Leader|Slaves], Group}, Slaves, Group)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.
