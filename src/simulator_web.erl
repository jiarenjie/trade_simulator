%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc  web服务对应的gen_server,从之前的gw_web迁移过来
%%%
%%% @end
%%% Created : 16. Apr 2016 16:22
%%%-------------------------------------------------------------------
-module(simulator_web).
-include_lib("eunit/include/eunit.hrl").
-author("simonxu").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3

]).

-define(SERVER, ?MODULE).
-define(C_ACCEPTORS, 100).
-define(DEFAULT_PORT, 8888).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  Routes = routes(),
  Dispatch = cowboy_router:compile(Routes),
  Port = port(),
  TransOpts = [{port, Port}],
  ProtoOpts = [{env, [{dispatch, Dispatch}]}
    , {middlewares, [cowboy_router, cowboy_handler]}],
  %%{ok, _} =
  CowBoyStarted = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
  case CowBoyStarted of
    {ok, _} ->
      ok;
    {already_started, _} ->
      ok;
    {Code, Reason} ->
      lager:error("cowboy started error!Code = ~p,Reason = ~p", [Code, Reason])

  end,

  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->

  %inets:stop(),

  io:format("gws_web terminated.~n", []),
  ok = application:stop(cowboy),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
static() -> {dir, "priv/static", mime()}.
n2o() -> {dir, "lib/n2o/priv", mime()}.
mime() -> [{mimetypes, cow_mimetypes, all}].

routes() ->
  [
    {'_', [

      % txn url
%%      , {"/pg/pay", wh_txn_mcht_pay, []}
      {"/pg/pay", wh_txn_mcht_pay_pt, []}
%%      , {"/pg/pay_succ", wh_txn_up_pay_front, []}
      , {"/pg/pay_succ", wh_txn_up_pay_front_pt, []}
%%      , {"/pg/pay_succ_info", wh_up_back_pay_succ, []}
      , {"/pg/pay_succ_info", wh_txn_up_pay_back_pt, []}
%%      , {"/pg/query", wh_txn_mcht_query, []}
      , {"/pg/query", wh_txn_mcht_query_pt, []}
%%      , {"/pg/refund", wh_txn_mcht_refund, []}
      , {"/pg/refund", wh_txn_mcht_refund_pt, []}

    ]}

  ].


port() ->
  {ok, Port} = application:get_env(cowboy, http_port),
  Port.


