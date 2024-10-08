-module(time).

% function for the Lamport clock
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() -> 0.

inc(_, T) -> 
    T + 1.

merge(Ti, Tj) -> 
    max(Ti, Tj).

leq(Ti, Tj) ->
    Ti =< Tj.

% Returns a clockthat can keep track of the nodes 
clock(Nodes) ->
    dict:from_list([{Node, 0} || Node <- Nodes]).

% Return a clock that has been updated given that we have
% received a log message from a node at a given time
update(Node, Time, Clock) ->
    dict:store(Node, Time, Clock).

% Returns true or false if it safe to log 
% an event that happened at a given time or not 
safe(Time, Clock)  ->
    dict:fold(fun(_, T, Acc) -> 
        leq(Time, T) and Acc
    end, true, Clock).