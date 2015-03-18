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
    gen_server:call(?MODULE, {echo, sanitize(Message)}, get_config(timeout)).

sanitize(Message) ->
    case is_newline_terminated(Message) of
        true  -> count_chars("\n", Message);
        false -> erlang:error(no_newline)
    end.

get_config(Name) ->
    {ok, Value} = application:get_env(echo_app, Name),
    Value.
   
is_newline_terminated([]) -> false;
is_newline_terminated([$\n]) -> true;
is_newline_terminated([_|T]) -> is_newline_terminated(T).

count_chars(Char, Message) -> 
    length([X || X <- Message, X == Char]).

%% gen_server callbacks

init(App) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, App}, [stream, {line, get_config(maxline)}]),
    {ok, #state{port=Port}}.

handle_call({echo, Msg}, _From, #state{port=Port}=State) ->
    port_command(Port, Msg),
    case collect_response(Port) of
        {response, Response} -> {reply, Response, State};
        timeout -> {stop, port_timeout, State}
    end.

collect_response(Port) -> collect_response(Port, [], []).

collect_response(Port, RespAcc, LineAcc) ->
    receive 
        {Port, {data, {eol, "OK"}}} ->
            {response, lists:reverse(RespAcc)};
        {Port, {data, {eol, Result}}} ->
            Line = lists:reverse([Result|LineAcc]),
            {response, lists:reverse([Line|RespAcc])};
        {Port, {data, {noel, Result}}} ->
            collect_response(Port, RespAcc, [Result|LineAcc])
    after get_config(timeout) ->
          timeout
    end.

handle_info({'EXIT', Port, Reason}, #state{port=Port}=State) ->
    {stop, {port_terminated, Reason}, State}.

terminate({{port_terminated, _Reason}, _State}) ->
    ok;
terminate({_Reason, #state{port=Port}=_State}) ->
    port_close(Port).
    
handle_cast(_Msg, State) -> {noreply, State}.
