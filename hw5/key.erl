-module(key).
-export([generate/0, between/3]).

generate() ->
    rand:uniform(1000000000).

between(Key, From, To) ->
    case From =:= To of
        true -> 
            true; % Full circle, anything is between From and To
        false ->
            if 
                From < To -> 
                    Key > From andalso Key =< To;
                From > To -> 
                    Key > From orelse Key =< To
            end
    end.