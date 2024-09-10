-module(rudy).
-export([start/1, start/2, stop/0]).

% params :
%   - Port : port number
%   - Delay: delay in ms
init(Port, Delay) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen, Delay),
            gen_tcp:close(Listen);
        {error, Error} ->
            io:format("Error in gen_tcp:listen/2: ~p~n", [Error])
    end.

% params :
%   - Listen : socket
%   - Delay: delay in ms
handler(Listen, Delay) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client, Delay),
            handler(Listen, Delay);
        {error, Error} ->
            io:format("Error in gen_tcp:accept/1: ~p~n", [Error])
    end.

% params :
%   - Client : socket
%   - Delay: delay in ms
request(Client, Delay) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request, Delay),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("Error in gen_tcp:recv/2: ~p~n", [Error])
    end,
    gen_tcp:close(Client).

% params :
%   - Request : request
%   - Delay: delay in ms
reply({{get, URI, _}, _, _}, Delay) ->
    timer:sleep(Delay),
    http:ok(URI).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port, 0) end)).
start(Port, Delay) ->
    register(rudy, spawn(fun() -> init(Port, Delay) end)).

stop() ->
    exit(whereis(rudy), "time to die").