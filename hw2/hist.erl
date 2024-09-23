-module(hist).
-export([new/1, update/3]).

% Create a new history
new(Name) -> 
    [{Name, -1}]. % Initialize with -1, meaning any positive message number will be new

% Update the history with a new message number for Node
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        false -> % 
            {new, [{Node, N} | History]};
        {Node, LastSeen} when N > LastSeen -> 
            {new, [{Node, N} | lists:keydelete(Node, 1, History)]};
        _ -> 
            old
    end.
