-module(custom_notifications).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    case cowboy_req:body_qs(Req) of
        {ok,PostBodyList,Req1} ->
            io:format("~nPostBodyList:~p",[PostBodyList]),
            case notifier:notify(PostBodyList) of
                ok ->
                    FinalResp1 = <<"<html><body>Notifications Stored Successfully </body></html>">>,
                    {ok,Req2} = cowboy_req:reply(200,[],FinalResp1,Req1),
                    {ok,Req2,State};
                {error,Reason} ->
                    error_logger:info_msg("Error in custome_notifications:~p",[Reason]),
                    FinalResp1 = <<"<html><body>Internal Error </body></html>">>,
                    {ok,Req2} = cowboy_req:reply(500,[],FinalResp1,Req1),
                    {ok,Req2,State}
            end;
        {error,Reason} ->
             error_logger:info_msg("Error in cowboy_req:~p",[Reason]),
             FinalResp1 = <<"<html><body>Internal Error </body></html>">>,
            {ok,Req2} = cowboy_req:reply(500,[],FinalResp1,Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
	ok.
