-module(rudy).
-export([start/1, stop/0]).

%% Initialize the server by opening a listening socket and starting the handler
init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen);
        {error, Error} ->
            io:format("Error in gen_tcp:listen/2: ~p~n", [Error])
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            spawn(fun() -> request(Client) end),
            handler(Listen);
        {error, Error} ->
            io:format("Error in gen_tcp:accept/1: ~p~n", [Error])
    end.

request(Client) ->
    case gen_tcp:recv(Client, 0) of
        {ok, Request} ->
            RequestParsed = http:parse_request(Request),
            Response = reply(RequestParsed),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("Error in gen_tcp:recv/2: ~p~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    http:ok(URI).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die").
