-module(test_eu).

-export([setup/1, setup_remote/3, broadcast/0, update/0, simulate_failure/0]).             

setup_remote(RemoteHost, RemoteCity, RemoteRegister) ->
    eu1 ! {add, RemoteCity, {RemoteRegister, RemoteHost}}. % Adding the link between the regions

setup(Host) ->
    routy:start(eu1, london), % The first router is the link node with the other region
    routy:start(eu2, paris),
    routy:start(eu3, madrid),
    routy:start(eu4, amsterdam),
    routy:start(eu5, copenhagen),
    routy:start(eu6, stockholm),
    routy:start(eu7, berlin),
    routy:start(eu8, prague),
    timer:sleep(200),

    % Adding the links between the routers
    eu1 ! {add, paris, {eu2, Host}},
    eu2 ! {add, london, {eu1, Host}},
    
    eu2 ! {add, madrid, {eu3, Host}},
    eu3 ! {add, paris, {eu2, Host}},

    eu2 ! {add, amsterdam, {eu4, Host}},
    eu4 ! {add, paris, {eu2, Host}},

    eu4 ! {add, copenhagen, {eu5, Host}},
    eu5 ! {add, amsterdam, {eu4, Host}},

    eu5 ! {add, stockholm, {eu6, Host}},
    eu6 ! {add, copenhagen, {eu5, Host}},

    eu4 ! {add, berlin, {eu7, Host}},
    eu7 ! {add, amsterdam, {eu4, Host}},

    eu7 ! {add, prague, {eu8, Host}},
    eu8 ! {add, berlin, {eu7, Host}},
    timer:sleep(200),

    % Spreading the link-state messages
    broadcast(),
    timer:sleep(200),

    % Updating the routers
    update(),
    timer:sleep(200),

    ok.

broadcast() ->
    eu1 ! broadcast,
    eu2 ! broadcast,
    eu3 ! broadcast,
    eu4 ! broadcast,
    eu5 ! broadcast,
    eu6 ! broadcast,
    eu7 ! broadcast,
    eu8 ! broadcast.

update() ->
    eu1 ! update,
    eu2 ! update,
    eu3 ! update,
    eu4 ! update,
    eu5 ! update,
    eu6 ! update,
    eu7 ! update,
    eu8 ! update.

% Simulate the shutdown of Amsterdam (eu4)
simulate_failure() ->
    routy:stop(eu4),
    timer:sleep(200),

    % Spreading the link-state messages
    broadcast(),
    timer:sleep(200),

    % Updating the routers
    update(),
    timer:sleep(200).