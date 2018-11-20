-module(cowboy_demo_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
			 {cowboyHandler_sup,{cowboyHandler_sup,start_link,[]},permanent,5000,supervisor,[cowboyHandler_sup]},
			 {cowboy_demo_sub_sup, {cowboy_demo_sub_sup, start_link, []},permanent, 5000, supervisor, [cowboy_demo_sub_sup]}
			],
	{ok, {{one_for_all, 1, 5}, Procs}}.
