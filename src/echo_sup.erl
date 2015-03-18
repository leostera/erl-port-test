-module(echo_sup).
-author(ostera).

-behavior(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

start_link(App) -> 
    supervisor:start_link(echo_sup, App).

init(App) ->
    {ok, {{one_for_one, 3, 10},
          [{echo, {echo, start_link, [App]},
            permanent, 10, worker, [echo]}]}}.
