-module(simulator_store_mnesia).
-include_lib("eunit/include/eunit.hrl").
-include("include/store.hrl").

-export([
  init/0
  , init_table/0
  , start/0
  , db_init/0
]).


init() ->
  {ok, Dir} = application:get_env(mnesia, dir),
  lager:debug("Mnesia dir = ~p", [Dir]),
  %% set path of mnesia
  application:set_env(mnesia, dir, Dir),
  init_table().

init_table() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  mnesia:create_schema([node()]),
  ok = mnesia:start(),
  db_init().

start() ->
  %io:format("Application = ~p~n",[application:get_application()]),
  {ok, Dir} = application:get_env(mnesia, dir),
  lager:debug("Mnesia dir = ~p", [Dir]),
  application:set_env(mnesia, dir, Dir),

  ok = mnesia:start().

%% Internal Functions
db_init() ->
  [
    {db_init(T), index_init(T)}
    || T <- [txn_log]
%%  mcht_txn_log, mchants,up_txn_log,
  ].

db_init(txn_log) ->

  {atomic, ok} = mnesia:create_table(
    txn_log,
    [   %%{index,[order_id]}

      {attributes, record_info(fields, txn_log)}
      %, {index, [mcht_index_key]}
      , {disc_copies, [node()]}
    ]).

index_init(mchants) ->
  {atomic, ok} = mnesia:add_table_index(mchants, mcht_full_name),
  ok;
index_init(mcht_txn_log) ->
  {atomic, ok} = mnesia:add_table_index(mcht_txn_log, mcht_txn_date),
  {atomic, ok} = mnesia:add_table_index(mcht_txn_log, settle_date),
  ok;
index_init(up_txn_log) ->
  {atomic, ok} = mnesia:add_table_index(up_txn_log, up_settleDate),
  {atomic, ok} = mnesia:add_table_index(up_txn_log, up_index_key),
  ok;

index_init(ums_reconcile_result) ->
  {atomic, ok} = mnesia:add_table_index(ums_reconcile_result, txn_date),
  ok;

index_init(_) ->
  ok.

