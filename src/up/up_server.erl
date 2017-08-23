%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 七月 2017 10:33
%%%-------------------------------------------------------------------
-module(up_server).
-author("pingjianwei").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { mcht_txn_seq, count}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(MchtId :: binary()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(MchtId) ->
  gen_server:start_link( ?MODULE, [MchtId], []).
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
init([MchtOrderId]) ->
  {ok, #state{mcht_txn_seq = MchtOrderId},0}.

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

handle_info(timeout, #state{mcht_txn_seq = MchtOrderId} = State) ->
  UPModle = get_orderId_info(MchtOrderId),
  lager:debug("UPModle = ~p~n",[UPModle]),
  send_back_notice(UPModle,MchtOrderId),
  {stop, normal,State};
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

send_back_notice(Modle,MchtOrderId) when is_list(Modle)->
  [{txn_log,_,_,_,UpMerID,UpTxnTime,UpOrderId,_,_,_}] =Modle,
  PostVals = [{<<"accessType">>,<<"0">>}
    ,{<<"bizType">>,<<"000201">>}
    ,{<<"certId">>,<<"69597475696">>}
    ,{<<"currencyCode">>,<<"156">>}
    ,{<<"encoding">>,<<"UTF-8">>}
    ,{<<"merId">>,UpMerID}
    ,{<<"orderId">>,UpOrderId}
    ,{<<"queryId">>,<<"201708051218403583118">>}
    ,{<<"reqReserved">>,<<"A2568E">>}
    ,{<<"respCode">>,<<"00">>}
    ,{<<"respMsg">>,<<"Success!">>}
    ,{<<"settleAmt">>,<<"100">>}
    ,{<<"settleCurrencyCode">>,<<"156">>}
    ,{<<"settleDate">>, xfutils:yesterday(mmdd)}
    ,{<<"signMethod">>,<<"01">>}
    ,{<<"traceNo">>,<<"358311">>}
    ,{<<"traceTime">>,<<"0805121840">>}
    ,{<<"txnAmt">>,<<"100">>}
    ,{<<"txnSubType">>,<<"01">>}
    ,{<<"txnTime">>,UpTxnTime}
    ,{<<"txnType">>,<<"01">>}
    ,{<<"version">>,<<"5.0.0">>}
    ,{<<"signature">>,<<"piXuP+gMnf8kbu9tzcDkHSc7nC5520IUk5vjFxGNusDdmhV08d6/csWYG6z3K5DRvZ+1CCVZckdCaVdoZ+GtMZcIvy0DSU290u1ik3oijHeeiSw8tFq/Qn2rVzKPJngxuhkhhtJXZ8qrjC+BQyltX9Y22gl5JjlXXGy2XOJ3cQTKAgp4niMO4vOHrHZlxsfASsQ1kLIqQ3CM1pi8cCcZ8Z13zqYskl1RfCD4ZM6HxdOJtQjJ2TFH3sxgN0p69ZwaWw+ht1KK9+yPxZ1g0nYN3nRuovA0dranDsfKm9QoyD6stQPFIA1/Dtu3CpKu0AVvUbGb5cGygKCEjxYsGnPRFg==">>}],
  PostString = xfutils:post_vals_to_string(PostVals),
  %lager:info("PostString=~p",[PostString]),
  Url = xfutils:get_path(trade_simulator,zr_back_notice),
  case httpc:request(post,{Url, [], "application/x-www-form-urlencoded", PostString}, [], []) of
    HTTPRESPONSE->
      lager:info("MchtOrderId =~p HTTPRESPONSE = ~p~n",[MchtOrderId,HTTPRESPONSE])
  end.

get_orderId_info(MchtOrderId) ->
  mnesia:dirty_read(txn_log,MchtOrderId).



