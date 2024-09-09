-module(rudy_further).
-export([start/1, start/2, start/3, stop/0]).

init(Port, Delay, Exit) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen, Delay, 0),
            gen_tcp:close(Listen),
            Exit(),
            ok;
        {error, Error} ->
            io:format("Error in gen_tcp:listen/2: ~p~n", [Error])
    end.

init(Port, Delay, P, Exit) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
        case gen_tcp:listen(Port, Opt) of
            {ok, Listen} ->
                Pids = handler_pool(Listen, Delay, P),
                receive 
                    done -> 
                        io:format("killing all spawned processes~n"),
                        % kill all the spawned processes
                        lists:foreach(fun(Pid) -> 
                            exit(Pid, kill) end,   
                        Pids),
                        Exit(),
                        ok
                end;
            {error, Error} ->
                io:format("Error in gen_tcp:listen/2: ~p~n", [Error])
        end.

handler_pool(Listen, Delay, P) ->
    Pids = create_handler_pool(Listen, Delay, P, []),
    receive
        done -> 
            Pids
    end.

create_handler_pool(Listen, Delay, P, L) ->
    case P of 
        0 -> 
            self() ! done,
            L;
        _ -> 
            Pid = spawn(fun() -> handler(Listen, Delay, P) end),
            create_handler_pool(Listen, Delay, P-1, [Pid|L])
    end.

handler(Listen, Delay, ID) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client, Delay, ID),
            handler(Listen, Delay, ID);
        {error, Error} ->
            io:format("Error in gen_tcp:accept/1: ~p~n", [Error])
    end.

request(Client, Delay, _) ->
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

reply({{get, URI, _}, _, _}, Delay) ->
    timer:sleep(Delay),
    http:ok(URI).

exit_rudy() ->
    io:format("exiting rudy~n"),
    exit("time to die").

start(Port) ->
    register(rudy, spawn(fun() -> init(Port, 0, fun exit_rudy/0) end)).
start(Port, Delay) ->
    register(rudy, spawn(fun() -> init(Port, Delay, fun exit_rudy/0) end)).
start(Port, Delay, P) ->
    register(rudy, spawn(fun() -> init(Port, Delay, P, fun exit_rudy/0) end)).

stop() ->
    rudy ! done.

