-module(logs).
-export([start/2, stop/1]).

start(Nodes, Config) ->
    spawn_link(fun() ->init(Nodes, Config) end).

stop(Logger) ->
    Logger ! stop.

% The logger is given a list of nodes that will send 
% messages to, but for now, we ignore this
init(Nodes, Config) ->
    io:format("initializing nodes: ~w~n", [Nodes]),

    case Config of
        % Part 1: Just log the message
        1 -> 
            Clock = na,
            QueueMaxSize = na;
        % Part 2: Lamport time & message queue
        2 ->
            Clock = time:clock(Nodes),
            QueueMaxSize = 0;
        % Part 3: Vector clock & message queue
        3 ->
            Clock = vect:clock(Nodes),
            QueueMaxSize = 0
    end,
    loop(Clock, [], QueueMaxSize, Config).


loop(Clock, LogsQueue, QueueMaxSize, Config) ->
    receive
        {log, From, Time, Msg} ->

            case Config of
                % Part 1: Just log the message
                1 -> 
                    NewClock = Clock,
                    NewLogsQueue = LogsQueue,
                    NewQueueMaxSize = na,
                    log(From, Time, Msg);
                % Part 2: Lamport time & message queue
                2 ->
                    NewClock = time:update(From, Time, Clock),
                    NewLogsQueue = log_queue(NewClock, [{Time, From, Msg} | LogsQueue], Config),
                    NewQueueMaxSize = max(QueueMaxSize, length(NewLogsQueue));
                % Part 3: Vector clock & message queue
                3 ->
                    NewClock = vect:update(From, Time, Clock),
                    NewLogsQueue = log_queue(NewClock, [{Time, From, Msg} | LogsQueue], Config),
                    NewQueueMaxSize = max(QueueMaxSize, length(NewLogsQueue))
            end,
            loop(NewClock, NewLogsQueue, NewQueueMaxSize, Config);

        stop ->
            if 
                QueueMaxSize /= na -> 
                    io:format("max queue size: ~w~n", [length(LogsQueue)]),
                    ok;
                true -> 
                    ok
            end,
            ok
        end.

% Part 1: Just log the message
log(From, Time, Msg) ->
    io:format("log: ~s ~w ~p~n", [Time, From, Msg]).


log_queue(Clock, LogsQueue, Config) ->
    case Config of
        2 -> 
            lists:foldl(fun({T, F, M}, Acc) -> 
                case time:safe(T, Clock) of
                    true -> 
                        io:format("log: ~w ~w ~p~n", [T, F, M]),
                        Acc;
                    false -> 
                        [{T, F, M} | Acc]
                end
            end, [], LogsQueue);
        3 -> 
            lists:foldl(fun({T, F, M}, Acc) -> 
                case vect:safe(T, Clock) of
                    true -> 
                        io:format("log: ~w ~w ~p~n", [T, F, M]),
                        Acc;
                    false -> 
                        [{T, F, M} | Acc]
                end
            end, [], LogsQueue)
    end.