-module(controler).


-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
         }).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {Method,Req2} = cowboy_req:method(Req),
  {ok,ReqFin}  =	 case Method of
                           <<"POST">> ->
                             case cowboy_req:header(<<"content-type">>,Req) of
                               {Form,Req0} when Form=:= <<"application/json; charset=UTF-8">>;Form =:= <<"application/json">> ->
                                 case cowboy_req:body(Req0) of
                                   {error,Reason} ->
                                     io:format("~n~p",[Reason]),
                                     reply(500,<<"text/html">>,<<"Internal Error">>,Req0);
                                   {ok,JSONBody,Req1} ->
                                     case catch jiffy:decode(JSONBody) of
                                       {JSONList} ->
                                         case processRequest(JSONList,Req1) of
                                           {error,Reason} ->
                                             ReasonFin = io_lib:print(Reason),
                                             reply(500,<<"text/html">>,list_to_binary(ReasonFin),Req1);
                                           {ok,Response,Req5} ->
                                             ResFin = io_lib:print(Response),
                                             reply(200,<<"text/html">>,list_to_binary(ResFin),Req5)
                                         end;
                                       _Any ->
                                         reply(500,<<"text/html">>,<<"Internal Error">>,Req1)
                                     end
                                 end;
                               _ ->
                                 reply(501,<<"text/html">>,<<"Service Not Implemented">>,Req2)
                             end;
                           _ ->
                             reply(501,<<"text/html">>,<<"Service Not Implemented">>,Req2)
                         end,
  {ok, ReqFin, State}.

terminate(_Reason, _Req, _State) ->
  ok.
%%% INTERNAL FUNCTIONS
reply(StatusCode,ContentType,FinalResp,Req) ->
  FinalResp1 =  <<"<html><body>",FinalResp/binary,"</body></html>">>,
  cowboy_req:reply(StatusCode,[{<<"content_type">>,ContentType}],FinalResp1,Req).

processRequest(JSONList,Req) ->
  case getArguments(JSONList,[])  of
    {error,Reason} ->
      {error,Reason};
    Args ->
      {Mod1,Req1}=cowboy_req:binding(module,Req),
      {Fun1,Req2}=cowboy_req:binding(function,Req1),
      Mod = erlang:binary_to_atom(Mod1,unicode),
      Fun = erlang:binary_to_atom(Fun1, unicode),
      io:format("~n~p:~p:~p",[Mod,Fun,Args]),
      case catch erlang:apply(Mod, Fun, Args) of
        {'EXIT',{Reason,_}} ->
          {error,Reason};
        {error,Reason} ->
          {error,Reason};
        Response ->
          {ok,Response,Req2}
      end
  end.

getArguments([],Rest) ->
  lists:reverse(Rest);
getArguments([{<<"string">>,Data}|Rest],List) ->
  getArguments(Rest,[binary_to_list(Data)|List]);
getArguments([{Type,Data}|Rest],List) when Type =:= <<"int">> ; Type =:= <<"float">> ->
  getArguments(Rest,[Data|List]);
getArguments(_,_) ->
  {error,"JSON DATA Type Error"}.
