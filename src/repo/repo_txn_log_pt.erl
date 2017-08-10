%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2017 1:43 PM
%%%-------------------------------------------------------------------
-module(repo_txn_log_pt).
%%-compile({parse_trans, exprecs}).
-behavior(behaviour_repo).
-author("simon").

%%-define(BH, behaviour_repo).
%% API
%% callbacks
-export([
  table_config/0
]).

-compile(export_all).
%%-------------------------------------------------------------
-define(TBL,txn_log).


-type txn_type() :: pay |refund|gws_up_query.
-type status() :: success |waiting |fail.
-type txn_amt() :: non_neg_integer().

-export_type([txn_type/0, status/0, txn_amt/0]).

-record(?TBL, {
  mcht_txn_seq
  , mcht_txn_date
  , mcht_txn_time
  , up_merId
  , up_txnTime
  , up_orderId
  , up_txnAmt
  , txn_statue
  , up_respCode
}).

-type ?TBL() :: #?TBL{}.
-export_type([?TBL/0]).

-export_records([?TBL]).
%%-------------------------------------------------------------
%% call backs
table_config() ->
  #{
%%    table_indexes => [up_index_key, up_settleDate]
     data_init => []
    , pk_is_sequence => false
    , pk_key_name => mcht_txn_seq
    , pk_type => tuple

%%    , unique_index_name => up_index_key
    , query_option =>
  #{
    up_txnAmt => integer_equal
  }

  }.
