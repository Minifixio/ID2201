-module(dijkstra).
-export([table/2, route/2]).

% Return the length of the shortest path to Node in Sorted
entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        false -> 0;
        {Node, Length, _} -> Length
    end.

% Replace the entry for Node in Sorted with N and Gateway
replace(Node, N, Gateway, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        false -> Sorted;
        {Node, _, _} ->
            Sorted1 = lists:keydelete(Node, 1, Sorted),
            lists:keysort(2, [{Node, N, Gateway} | Sorted1])
    end.

% Update the shortest path to Node in Sorted with N and Gateway
update(Node, N, Gateway, Sorted) ->
    case entry(Node, Sorted) of
        0 -> Sorted;
        L -> 
            case N < L of
                true -> replace(Node, N, Gateway, Sorted);
                false -> Sorted
            end
    end.

% Iterate over Sorted, updating the shortest paths in Table
iterate(Sorted, Map, Table) ->
    case Sorted of
        [] -> Table;
        [{_, inf, _} | _] -> Table;
        [{Node, Length, Gateway} | Rest] ->
            Links = map:reachable(Node, Map),
            NewSorted = lists:foldl(
                fun(N, Acc) ->
                    Res = update(N, Length + 1, Gateway, Acc),
                    Res
                end,
            Rest, Links),
            iterate(NewSorted, Map, [{Node, Gateway} | Table])
    end.

% Create a routing table from Gateways and Map
table(Gateways, Map) ->
    Sorted = lists:keysort(2, 
        lists:foldl(
            fun(N, Acc) -> 
                case lists:member(N, Gateways) of
                    true -> [{N, 0, N} | Acc];
                    false -> [{N, inf, unknown} | Acc]
                end
        end, [], map:all_nodes(Map))
    ),
    iterate(Sorted, Map, []).

% Return the gateway to Node in Table
route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
        false -> notfound;
        {Node, Gateway} -> {ok, Gateway}
    end.