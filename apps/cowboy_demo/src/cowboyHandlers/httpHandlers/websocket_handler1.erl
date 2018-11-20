-module(websocket_handler1).


-export([init/3,
        websocket_init/3,
        websocket_handle/3,
        websocket_info/3,
        websocket_terminate/3]).


init(_, _Req, _Opts) ->
    io:format("~nTest"),
	{upgrade,protocol,cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
        io:format("Webscoket:~p",[self()]),
            {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    io:format("~nTestMsgReceived:~p",[Msg]),
        {reply, {text, << "Echo From Web Socket:: ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    io:format("~nData:~p",[_Data]),
        {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    io:format("~nTimeOut:~p",[Msg]),
        erlang:start_timer(1000, self(), <<"How' you doin'?">>),
            {reply, {text, Msg}, Req, State};
websocket_info(Info,Req,State) when is_list(Info) ->
  Msg = erlang:list_to_binary(Info),
  {reply,{text,Msg},Req,State};
websocket_info(_Info, Req, State) ->
    io:format("~nInfoMsg:~p",[_Info]),
        {ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
  io:format("Web Socket Terminated. Reason:~p",[Reason]),
        ok.

