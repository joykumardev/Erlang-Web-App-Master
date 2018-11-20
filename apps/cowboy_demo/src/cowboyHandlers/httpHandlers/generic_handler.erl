-module(generic_handler).


-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
%	io:format("~n\t\tInitialRequest:~p",[timer:tc(cowboy_req,to_list,[Req])]),
	{Method, Req2} = cowboy_req:method(Req),
	{FinalResp,StatusCode,CT,Req4}= processReq(Method,Req2),
	FinalResp1 = <<"<html><body>",FinalResp/binary,"</body></html>">>,
	{ok,Req5} = cowboy_req:reply(StatusCode,[{<<"content_type">>,CT}],FinalResp1,Req4),
%	io:format("~n\t\t FinalRequest:~p",[cowboy_req:to_list(Req5)]),
	{ok, Req5, State}.
%% GET request handler
processReq(<<"GET">>,Req) ->
	{QueryList,Req0}=cowboy_req:qs_vals(Req),
	QueryBinary =genHtmlFromListTuple(QueryList),
	{<<"GET Request body<br>",QueryBinary/binary>>,200,<<"text/html">>,Req0};
	
%% POST request handler	
processReq(<<"POST">>,Req) ->
	case cowboy_req:header(<<"content-type">>,Req) of
		{Form,Req0} when Form=:= <<"application/x-www-form-urlencoded">>;Form=:= <<"application/x-www-form-urlencoded; charset=UTF-8">> ->
			case cowboy_req:body_qs(Req0) of
				{ok,PostBodyList,Req1} ->
					Binary = genHtmlFromListTuple(PostBodyList),
					{<<"POST Request body<br />",Binary/binary>>,200,<<"text/html">>,Req1};
				{error,_Reason} ->
					{<<"Internal Eror">>,500,<<"text/html">>,Req}
			end;
		{Form,Req0} when Form=:= <<"application/json; charset=UTF-8">>;Form =:= <<"application/json">>->
			case cowboy_req:body(Req0) of
				{error,Reason} ->
					io:format("~n~p",[Reason]),
					{<<"Internal Eror">>,500,<<"text/html">>,Req0};
				{ok,JSONBody,Req1} ->
					case catch jiffy:decode(JSONBody) of
						{JSONList} ->
							Binary = genHtmlFromListTuple(JSONList),
							{<<"JSON Request body<br />",Binary/binary>>,200,<<"text/html">>,Req1};
						Any ->
							io:format("~nJSON Error:~p",[Any]),
							{<<"Internal Eror">>,500,<<"text/html">>,Req0}
					end
			end;
		_ ->
			{<<"Service Not Implemented">>,501,<<"text/html">>,Req}			
	end;
processReq(_,Req) ->
	{<<"Service Not Implemented">>,501,<<"text/html">>,Req}.
	

terminate(_Reason, _Req, _State) ->
	ok.
%%%%%%% Internals %%%%%%%%%%%%%%%%%%%%%%%%%
% Generates html content from list of tuples
genHtmlFromListTuple(List) ->
	Fun = fun({Term,Val},Acc) ->
			Bin= <<Term/binary,":",Val/binary>>,
			<<Acc/binary,"<br />",Bin/binary>>
		end,
	lists:foldl(Fun,<<>>,List).

