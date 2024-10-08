-module(worker).
-export([start/6, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter, Config) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter, Config) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter, Config) ->
    rand:seed(exs1024s, [Seed, Seed, Seed]),

    case Config of
        % Part 1: Time has no initial value as it is defined as the current time
        1 -> 
            Time = na;
        % Part 2: Lamport time
        2 ->
            Time = time:zero();
        % Part 3: Vector clock
        3 ->
            Time = vect:zero()
    end,

    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, Time, Config);
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Time, Config) ->
    Wait = rand:uniform(Sleep),
    receive
        {msg, TimeRcv, Msg} ->

            case Config of
                % Part 1: Just the time the message was received
                1 -> 
                    NewTime = TimeRcv;
                % Part 2: Lamport time
                2 ->
                    NewTimeTemp = time:merge(TimeRcv, Time),
                    NewTime = time:inc(Name, NewTimeTemp);
                % Part 3: Vector clock
                3 ->
                    NewTime = vect:merge(TimeRcv, Time)
            end,

            Log ! {log, Name, NewTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, NewTime, Config);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
        after Wait ->
            Selected = select(Peers),

            case Config of
                % Part 1: Default timestamp is the current time
                1 -> 
                    {_, {Hour, Minute, Second}} = calendar:local_time(),
                    NewTime = io_lib:format("~p:~p:~p", [Hour, Minute, Second]);
                % Part 2: Timestamp is the Lamport clock
                2 ->
                    NewTime = time:inc(Name, Time);
                % Part 3: Timestamp is the vector clock
                3 ->
                    NewTime = vect:inc(Name, Time)
            end,

            Message = {hello, rand:uniform(100)},
            Selected ! {msg, NewTime, Message},
            
            jitter(Jitter),
            
            Log ! {log, Name, NewTime, {sending, Message}},

            loop(Name, Log, Peers, Sleep, Jitter, NewTime, Config)
    end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> 
    ok;
jitter(Jitter) -> 
    timer:sleep(rand:uniform(Jitter)).