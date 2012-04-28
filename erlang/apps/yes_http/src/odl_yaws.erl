-module(odl_yaws).

-include("../../include/node.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%gen_server
-export([start/2, init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

start(_T, _A) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


