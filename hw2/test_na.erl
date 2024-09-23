-module(test_na).

-export([setup/1, setup_remote/3, broadcast/0, update/0, simulate_failure/0]).             

setup_remote(RemoteHost, RemoteCity, RemoteRegister) ->
    na1 ! {add, RemoteCity, {RemoteRegister, RemoteHost}}. % Adding the link between the regions

setup(Host) ->
    routy:start(na1, new_york), % The first router is the link node with the other region
    routy:start(na2, toronto),
    routy:start(na3, montreal),
    routy:start(na4, denver),
    routy:start(na5, austin),
    routy:start(na6, san_francisco),
    routy:start(na7, seattle),
    timer:sleep(200),

    % Adding the links between the routers
    na1 ! {add, toronto, {na2, Host}},
    na2 ! {add, new_york, {na1, Host}},

    na2 ! {add, montreal, {na3, Host}},
    na3 ! {add, toronto, {na2, Host}},

    na1 ! {add, denver, {na4, Host}},
    na4 ! {add, new_york, {na1, Host}},

    na4 ! {add, austin, {na5, Host}},
    na5 ! {add, denver, {na4, Host}},
    na4 ! {add, seattle, {na7, Host}},
    na7 ! {add, denver, {na4, Host}},

    na4 ! {add, san_francisco, {na6, Host}},
    na6 ! {add, austin, {na5, Host}},

    na6 ! {add, seattle, {na7, Host}},
    na7 ! {add, san_francisco, {na6, Host}},
 
    timer:sleep(200),

    % Spreading the link-state messages
    broadcast(),
    timer:sleep(200),

    % Updating the routers
    update(),
    timer:sleep(200),

    ok.

broadcast() ->
    na1 ! broadcast,
    na2 ! broadcast,
    na3 ! broadcast,
    na4 ! broadcast,
    na5 ! broadcast,
    na6 ! broadcast,
    na7 ! broadcast.

update() ->
    na1 ! update,
    na2 ! update,
    na3 ! update,
    na4 ! update,
    na5 ! update,
    na6 ! update,
    na7 ! update.

% Simulate the shutdown of Denver (na4)
simulate_failure() ->
    routy:stop(na4),
    timer:sleep(200),

    % Spreading the link-state messages
    broadcast(),
    timer:sleep(200),

    % Updating the routers
    update(),
    timer:sleep(200).