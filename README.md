cowboy_demo
================================================================================

cowboy_demo is an OTP application to test the below features of Cowboy HTTP server.

Features:
    1. GET Request
    2. POST Request
    3. POST Request with JSON content
    4. Binding atoms in http path to variables
    5. Websockets

System Requirements:
    1. Erlang/OTP 19+
    2. Rebar3


Build
=================================================================================

    $ rebar3 clean
    $ rebar3 compile
    $ rebar3 shell  -- to run the application on foreground


Usage
=================================================================================
GET Request:
-------------
curl -X GET http://localhost:8080/erlang
Hello Erlang!

curl -X GET "http://localhost:8080/get?user=abc&password=123"
<html><body>GET Request body<br><br />user:abc<br />password:123</body></html>

POST Request:
---------------
curl -X POST -d "user=xyz&password=321" http://localhost:8080/post
<html><body>POST Request body<br /><br />user:xyz<br />password:321</body></html>


POST Request with JSON:
-----------------------
curl -X POST -H "Content-Type: application/json" -d '{"key1":"val1", "key2":"val2"}' http://localhost:8080/post
<html><body>JSON Request body<br /><br />key1:val1<br />key2:val2</body></html>


Binding atoms in http path to variables:
-----------------------------------------
curl -X POST -H "Content-Type: application/json" -d '{"int":1, "int":5}' http://localhost:8080/lists/seq
<html><body>[1, 2, 3, 4, 5]</body></html>

curl -X POST -H "Content-Type: application/json" -d '{}' http://localhost:8080/erlang/time
<html><body>{21, 44, 41}</body></html>

 curl -X POST -H "Content-Type: application/json" -d '{}' http://localhost:8080/erlang/system_time
<html><body>1528993070376529388</body></html>


Web Sockets:
-------------
 wscat -c ws://localhost:8080/websocket
connected (press CTRL+C to quit)
> TestMessage
< Echo From Web Socket:: TestMessage
> Second Message.............
< Echo From Web Socket:: Second Message.............
> q
< Echo From Web Socket:: q
>

