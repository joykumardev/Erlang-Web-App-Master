-define(PORT,8080).
-define(PATHS_LIST,[
                    {"/", cowboy_static, {priv_file,cowboy_demo, "index.html"}},
                    {"/",cowboy_demo_handler,[]},
                    {"/erlang",cowboy_demo_handler,[]},
                    {"/get",generic_handler,[]},
                    {"/post",generic_handler,[]},
                    {"/websocket",websocket_handler1,[]},
                    {"/:module/:function",controler,[]},
                    {"/notifications",custom_notifications,[]}
                   ]).
-define(TRANSPORT_OPTS,[{port,?PORT}]).
-define(PROTOCOL_OPTIONS,[
                          {onresponse, fun cowboy_access_logger:log/4}
                         ]).
-define(CLIENT_ACCEPTORS,100).
