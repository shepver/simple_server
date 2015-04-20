%%%-------------------------------------------------------------------
%%% @author shepver
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Март 2015 17:05
%%%-------------------------------------------------------------------
-module(simple_server).
-author("shepver").

%% API
-export([start/0]).

-define(PORT, 48260).

start() ->
  application:start(ranch),
  ranch:start_listener(my_pool, 1,
    ranch_tcp, [{port, ?PORT}],
    server_protocol_handler, []),
  application:start(simple_server).