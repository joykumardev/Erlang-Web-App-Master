-module(notifier).

-behaviour(gen_server).

%% API
-export([start_link/0,notify/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 60*1000).

%%%-record(state, []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
notify(List) ->
    Notification = proplists:get_value(<<"name">>,List,<<"Default">>),
    Time = proplists:get_value(<<"time">>,List,<<"00:00">>),
    [H,M] = string:tokens(erlang:binary_to_list(Time),":"),
    HH = string:right(H,2,$0),
    MM = string:right(M,2,$0),
    gen_server:call(?SERVER,{notify,erlang:binary_to_list(Notification),{HH,MM}}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
        {ok, [],?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({notify,Notification,Time},_From,State) ->
    {reply,ok,[{Notification,Time}|State],?TIMEOUT};
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State,?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
        {noreply, State,?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout,State) ->
    io:format("~nStateTest:~p",[State]),
    spawn(fun()->processState(State)end),
    {noreply,State,?TIMEOUT};
handle_info(_Info, State) ->
        {noreply, State,?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
processState([]) ->
    io:format("process completed"),
    ok;
processState([{Note,{Def_H,Def_M}}|Rest]) ->
    {H,M,_}=erlang:time(),
    HStr = string:right(erlang:integer_to_list(H),2,$0),
    MStr = string:right(erlang:integer_to_list(M),2,$0),
    if
        (  Def_H =:= HStr ) and ( Def_M =:= MStr ) ->
            Result = os:cmd("notify-send '"++Note++"'"),
            io:format("~nNotifySendResult:~p",[Result]),
            ok;
        true ->
            ok
    end,
    processState(Rest).




