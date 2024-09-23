-module(test).
-export([run/3]).

run(Sleep, Jitter, Config) ->
    Log = logs:start([john, paul, ringo, george], Config),

    A = worker:start(john, Log, 13, Sleep, Jitter, Config),
    B = worker:start(paul, Log, 23, Sleep, Jitter, Config),
    C = worker:start(ringo, Log, 36, Sleep, Jitter, Config),
    D = worker:start(george, Log, 49, Sleep, Jitter, Config),

    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),

    timer:sleep(5000),

    logs:stop(Log),
    
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).