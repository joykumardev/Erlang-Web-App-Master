-module(cowboy_access_logger).
-behaviour(gen_server).

%% API.
-export([start_link/0,log/4]).
-define(LOG_DIR,"Cowboy_logs/").

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	fd,
	fileName
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).
log(Status,_Headers,_Body,Req) ->
	StatusBin = integer_to_binary(Status),
	{HostBin,_} = cowboy_req:host(Req),
	DateTimeBin = getDateTime(),
	{MethodBin,_} = cowboy_req:method(Req),
	{PathBin,_} = cowboy_req:path(Req),
	{Version,_} = cowboy_req:version(Req),
	VerBin = erlang:atom_to_binary(Version,unicode),
	{UABin,_} = cowboy_req:header(<<"user-agent">>,Req, <<>>),
	List = [HostBin,DateTimeBin,MethodBin,VerBin,PathBin,StatusBin,UABin],
	FinalLogRec = string:join(lists:map(fun(T)->binary_to_list(T)end,List),"::"),
%	io:format("~p\n~p",[List,FinalLogRec]),
	catch gen_server:cast(?MODULE, {log,FinalLogRec}),
	Req.
%% gen_server.

init([]) ->
	case filelib:ensure_dir(?LOG_DIR) of
		ok ->
			FileName = getLogFileName(),
			FinFile = ?LOG_DIR++FileName,
			case file:open(FinFile, [append]) of
				{ok,IoDevice} ->
					{ok,#state{fd=IoDevice,fileName=FinFile}};
				{error,Reason} ->
					{error,Reason}
			end;
		{error,Reason} ->
			{error,Reason}
	end.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.
handle_cast({log,FinalLog},State) ->
%	io:format("~s",[FinalLog++"\n"]),
	catch io:format(State#state.fd,"~s",[FinalLog++"\n"]),
	{noreply,State};
handle_cast(_Msg, State) ->
	{noreply, State}.
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
%% Internal Functions
getLogFileName()->
	{Y,M,_}=erlang:date(),
	FileName = erlang:integer_to_list(Y)++"-"++string:right(integer_to_list(M),2,$0)++".txt",
	FileName.
getDateTime() ->
	{{Y,M,D},{H,Mi,S}} = erlang:localtime(),
	Year = erlang:integer_to_list(Y),
	Month = string:right(erlang:integer_to_list(M),2,$0),
	Day = string:right(erlang:integer_to_list(D),2,$0),
	Date = string:join([Year,Month,Day],"/"),
	Hour = string:right(erlang:integer_to_list(H),2,$0),
	Min = string:right(erlang:integer_to_list(Mi),2,$0),
	Sec = string:right(erlang:integer_to_list(S),2,$0),
	list_to_binary(string:join([Date,Hour,Min,Sec], ":")).
	
	
