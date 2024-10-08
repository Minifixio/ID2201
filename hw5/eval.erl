-module(eval).
-export([setup1/1, setup2/1, performance/2]).
-define(LookupTimeout, 1000).

setup1(N) ->
    case N of
        1 -> 
            N1 = node1:start(1),
            N1;
        _ ->
            Node = setup1(N-1),
            node1:start(N, Node)
    end.

setup2(N) ->
    case N of
        1 -> 
            N1 = node2:start(1),
            io:format("Setting up node ~p | ~p~n", [N, N1]),
            {N1, [N1]};
        _ ->
            {Node, Prev} = setup2(N-1),
            NewNode = node2:start(N, Node),
            io:format("Setting up node ~p | ~p~n", [N, NewNode]),
            {NewNode, [NewNode | Prev]}
    end.


add(Receiver, Sender, K) ->
    case K of
        0 -> 
            [];
        _ -> 
            Key = key:generate(),
            Value = key:generate(),
            Receiver ! {add, Key, Value, make_ref(), Sender},
            [Key | add(Receiver, Sender, K-1)]
    end.

add_multiple(Nodes, TestNodes, K) ->
    case {Nodes, TestNodes} of
        {[], []} -> 
            [];
        {[M | RestNodes], [T | RestTest]} -> 
            Keys = add(M, T, K),
            [Keys | add_multiple(RestNodes, RestTest, K)]
    end.

lookup(Node, From, Keys) ->
    case Keys of
        [] -> 
            ok;
        [Key | Rest] -> 
            Q = make_ref(),
            Node ! {lookup, Key, Q, From},
            receive 
                {Q, _} ->
                    lookup(Node, From, Rest)
                after ?LookupTimeout ->
                    {error, "timeout"}
            end
    end.

lookup_multiple(Nodes, From, Keys) ->
    case {Nodes, Keys} of
        {[], []} -> 
            [];
        {[M | RestNodes], [Ks | RestKeys]} -> 
            lookup(M, From, Ks),
            lookup_multiple(RestNodes, From, RestKeys)
    end.

% eval:performance(4, 4000).
performance(N, K) ->
    % N testing nodes that will add K/N keys each
    io:format("--------~n"),
    io:format("Setting up ~p test nodes~n", [N]),
    {_, TestNodes} = setup2(N),
    io:format("TestNodes: ~p~n", [TestNodes]),
    io:format("--------~n~n"),
    timer:sleep(1000),


    % Test with 1 node in the ring that stores the K keys
    io:format("--------~n"),
    io:format("Testing with 1 node~n"),
    {MainNode, _} = setup2(1),
    io:format("MainNode: ~p~n~n", [MainNode]),
    timer:sleep(1000),

    % Each node of Nodes will add K/N keys to MainNode and we gather the keys in a list
    StartAdd1 = erlang:system_time(micro_seconds),
    Keys1 = lists:flatten(lists:map(fun(Node) -> add(MainNode, Node, K div N) end, TestNodes)),
    % io:format("Keys1: ~p~n", [Keys1]),
    EndAdd1 = erlang:system_time(micro_seconds),
    io:format("Adding ~p keys took ~p μs~n", [K, EndAdd1-StartAdd1]),
    timer:sleep(1000),

    % Test lookup of all keys
    StartLookup1 = erlang:system_time(micro_seconds),
    lookup(MainNode, self(), Keys1),
    EndLookup1 = erlang:system_time(micro_seconds),
    io:format("Looking up ~p keys took ~p μs~n", [K, EndLookup1-StartLookup1]),
    io:format("--------~n~n"),


    % Now test with N nodes in the ring that stores the K keys
    io:format("--------~n"),
    io:format("Testing with ~p nodes~n", [N]),
    {_, MainNodes} = setup2(N),
    io:format("MainNodes: ~p~n~n", [MainNodes]),
    timer:sleep(4000),

    % Each test node will add K/N keys to each node in MainNodes
    StartAdd2 = erlang:system_time(micro_seconds),
    Keys2 = add_multiple(MainNodes, TestNodes, K div N),
    % io:format("Keys2: ~p~n", [Keys2]),
    EndAdd2 = erlang:system_time(micro_seconds),
    io:format("Adding ~p keys took ~p μs~n", [K, EndAdd2-StartAdd2]),
    timer:sleep(1000),

    % Test lookup of all keys
    StartLookup2 = erlang:system_time(micro_seconds),
    lookup_multiple(MainNodes, self(), Keys2),
    EndLookup2 = erlang:system_time(micro_seconds),
    io:format("Looking up ~p keys took ~p μs~n", [K, EndLookup2-StartLookup2]),
    io:format("--------~n~n").




    



    

