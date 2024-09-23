-module(intf).
-export([
    new/0, 
    add/4, 
    remove/2, 
    lookup/2, 
    ref/2, 
    name/2, 
    list/1, 
    broadcast/2
]).

% Create a new interface
new() -> [].

% Add a new node to the interface
add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | Intf].

% Remove a node from the interface
remove(Name, Intf) ->
    lists:filter(fun({N, _, _}) -> N =/= Name end, Intf).

% Lookup a node in the interface based on its name
% Return {ok, Pid} if found, notfound otherwise
lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {_, _, Pid} -> {ok, Pid};
        false -> notfound
    end.

% Find the reference of a node in the interface
ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {_, Ref, _} -> {ok, Ref};
        false -> notfound
    end.

% Find the name of a node in the interface
name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        {Name, _, _} -> {ok, Name};
        false -> notfound
    end.

% List all nodes in the interface
list(Intf) ->
    [Name || {Name, _, _} <- Intf].

% Broadcast a message to all nodes in the interface
broadcast(Message, Intf) ->
    lists:foreach(fun({_, _, Pid}) -> Pid ! Message end, Intf).
