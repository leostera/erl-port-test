-module(echo).
-author(ostera).

%% External exports
-export([start_link/1]).

%% API exports
-export([echo/1]).

%% gen_server callback exports
-export([
         init/1,
         handle_call/3,
         handle_info/2,
         handle_cast/2,
         code_changes/3,
         terminate/2
        ]).

%% records
-record(state, {port}).

%% External
start_link(ExternalProgram) ->
    gen_server:start_link({local, ?MODULE}, echo, ExternalProgram).

%% API

echo(Message) ->
    gen_server:call(?MODULE, {echo, sanitize(Message)}, get_timeout()).

sanitize(Message) ->
    case is_newline_terminated(Message) of
        true  -> count_chars("\n", Message);
        false -> erlang:error(no_newline)
    end.

get_timeout() ->
    {ok, Value} = application:get_env(echo_app, timeout),
    Value.

get_maxline() ->
    {ok, Value} = application:get_env(echo_app, maxline),
    Value.
   
is_newline_terminated([]) -> false;
is_newline_terminated([$\n]) -> true;
is_newline_terminated([_|T]) -> is_newline_terminated(T).

count_chars(Char, Message) -> 
    length([X || X <- Message, X == Char]).

%% gen_server callbacks

init(App) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, App}, [stream, {line, get_maxline()}]),
    {ok, #state{port=Port}}.
