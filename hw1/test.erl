-module(test).
-export([bench/2, bench/4]).

% bench function for a single process
bench(Host, Port) ->
    Start = erlang:system_time(micro_seconds),
    Parent = self(),
    run(100, Host, Port, Parent),
    Finish = erlang:system_time(micro_seconds),
    Finish - Start.

% bench function for multiple processes
% params :
%   - P : number of processes
%   - N : number of requests per process
bench(Host, Port, P, N) ->
    Start = erlang:system_time(micro_seconds),
    Parent = self(),
    spawn_parallel(P, N, Host, Port, Parent),
    wait_for_parallel(P),
    Finish = erlang:system_time(micro_seconds),
    Finish - Start.

% recursively spawn P processes that each make N requests
% params :
%   - P : number of processes
%   - N : number of requests per process
%   - Host : host
%   - Port : port
%   - Parent : parent process
spawn_parallel(P, N, Host, Port, Parent) ->
    case P of
        0 -> 
            ok;
        _ -> 
            spawn(fun() -> 
                    run(N, Host, Port, Parent), 
                    Parent ! done end
                ), 
            spawn_parallel(P-1, N, Host, Port, Parent)
    end.

% wait for all processes to finish (done message must be sent P times)
% params :
%   - P : number of processes
wait_for_parallel(P) ->
    case P of
        0 -> 
            ok;
        _ ->
            receive
                done -> 
                    wait_for_parallel(P-1)
            end
    end.

% recursively make N requests
% when done, send done message to Parent
% params :
%   - N : number of requests
%   - Host : host
%   - Port : port
%   - Parent : parent process
run(N, Host, Port, Parent) ->
        if
            N == 0 ->
                io:format("test: done~n");
                ok;
            true ->
                request(Host, Port),
                run(N-1, Host, Port, Parent)
        end.

% make a request to the server
% params :
%   - Host : host
%   - Port : port
request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("foo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
        {ok, _} ->
            ok;
        {error, Error} ->
            io:format("test: error: ~w~n", [Error])
    end,
        gen_tcp:close(Server).

