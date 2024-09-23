-module(test).

-export([setup/1, broadcast/0, update/0, simulate_failure/0]).

%
% Tasks of the test procedure :
% 1. Setup an automated test that sets up a network with at least 5 routers.
% 2. Compute properly a routing table using the Dijkstra algorithm.
% 3. Prove that the network that can compute the new shortest paths when a router is shut down
%


%                  +----------+                   
%                  |Lille (r2)|                   
%                  +----------+                   
%                     |                           
%                     |                           
% +-----------+    +----------+                   
% |Nantes (r3)|----|Paris (r1)|----+              
% +-----------+    +----------+    |              
%         |             |        +---------+      
%         |             |        |Lyon (r4)|-+    
% +-------------+       |        +---------+ |    
% |Bordeaux (r5)|       |                    |    
% +-------------+       |      +--+--------------+
%         |             |      |  |Marseille (r6)|
%         |      +------|------|  +--------------+
%         +------|Toulouse (r7)|                  
%                +-------------+                  


% Setup an automated test that sets up a network with at least 5 routers.
setup(Host) ->

    routy:start(r1, paris),
    routy:start(r2, lille),
    routy:start(r3, nantes),
    routy:start(r4, lyon),
    routy:start(r5, bordeaux),
    routy:start(r6, marseille),
    routy:start(r7, toulouse),
    timer:sleep(200),

    % Adding the links between the routers
    r1 ! {add, lille, {r2, Host}},
    r1 ! {add, lyon, {r4, Host}},
    r1 ! {add, nantes, {r3, Host}},
    r1 ! {add, toulouse, {r7, Host}},

    r2 ! {add, paris, {r1, Host}},
    
    r4 ! {add, paris, {r1, Host}},
    r4 ! {add, marseille, {r6, Host}},

    r3 ! {add, paris, {r1, Host}},
    r3 ! {add, bordeaux, {r5, Host}},

    r5 ! {add, nantes, {r3, Host}},
    r5 ! {add, toulouse, {r7, Host}},

    r7 ! {add, bordeaux, {r5, Host}},
    r7 ! {add, marseille, {r6, Host}},

    r6 ! {add, toulouse, {r7, Host}},
    r6 ! {add, lyon, {r4, Host}},
    timer:sleep(200),

    % Spreading the link-state messages
    broadcast(),
    timer:sleep(200),

    % Updating the routers
    update(),
    timer:sleep(200),

    ok.

broadcast() ->
    r1 ! broadcast,
    r2 ! broadcast,
    r3 ! broadcast,
    r4 ! broadcast,
    r5 ! broadcast,
    r6 ! broadcast,
    r7 ! broadcast.

update() ->
    r1 ! update,
    r2 ! update,
    r3 ! update,
    r4 ! update,
    r5 ! update,
    r6 ! update,
    r7 ! update.

% Simulate the shutdown of Lyon (r4)
simulate_failure() ->
    routy:stop(r4),
    timer:sleep(200),

    % Spreading the link-state messages
    broadcast(),
    timer:sleep(200),

    % Updating the routers
    update(),
    timer:sleep(200).