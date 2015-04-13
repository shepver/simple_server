-module(server_protocol_handler).
-behaviour(gen_server).
%%-behaviour(ranch_protocol).


%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIMEOUT, 10000).

-record(state, {socket, transport}).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
  proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
  log("init_protokol.~n"),
  ok = proc_lib:init_ack({ok, self()}),
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, true}, {packet, 0}, binary]),
  gen_server:enter_loop(?MODULE, [], #state{socket = Socket, transport = Transport}, ?TIMEOUT).

handle_info({tcp, _Socket, Data}, State) ->
  log("Incoming data ~p .~n", [Data]),
  {noreply, State, ?TIMEOUT};

handle_info({tcp_closed, _Socket}, State) ->
  log("stop tcp closed.~n"),
  {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
  log("stop tcp error.~n"),
  {stop, Reason, State};

handle_info(timeout, State) ->
  log("stop timeout.~n"),
  {stop, normal, State};

handle_info(_Info, State) ->
  log("stop unknow.~n"),
  {stop, normal, State}.

handle_call(info, _From, State) ->
  {reply, State, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  log("stop terminate.~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal.

log(Msg) ->
  error_logger:info_msg(Msg).
log(Msg, Opt) ->
  error_logger:info_msg(Msg, Opt).
