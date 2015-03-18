-module(echo_app).
-author(ostera).

-behavior(application).

%% application callbacks

-export([
         start/2,
         stop/1
        ]).

start(_Type, _Args) ->
    PrivDir = code:priv_dir(echo_app),
    {ok, App} = application:get_env(echo_app, app),
    echo_sup:start_link(filename:join([PrivDir, App])).
