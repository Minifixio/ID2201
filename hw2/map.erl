-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() -> [].

update(Node, Links, Map) ->
    case lists:keyfind(Node, 1, Map) of
        false -> [{Node, Links} | Map];
        {Node, _} -> [{Node, Links} | lists:keydelete(Node, 1, Map)]
    end.

reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        false -> [];
        {Node, Links} -> Links
    end.

all_nodes(Map) ->
    L = [],
    lists:foldl(fun({Node, Links}, Acc) -> 
        NewAcc = lists:foldl(fun(N, Acc2) -> 
                    case lists:member(N, Acc2) of
                        true -> Acc2;
                        false -> [N | Acc2]
                    end
                end, Acc, Links),
        case lists:member(Node, Acc) of
            true -> NewAcc;
            false -> [Node | NewAcc]
        end
    end, L, Map).