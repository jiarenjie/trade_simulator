%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2017 3:57 PM
%%%-------------------------------------------------------------------
-module(protocol_mcht_resp_pay).
-compile({parse_trans, exprecs}).
-author("simon").
-include("../../include/type_binarys.hrl").
-include("../../include/type_mcht_protocol.hrl").
-behavior(behaviour_protocol_model).

%% API
-export([
  model_type/0
  , out_2_model_config/0
  , model_2_out_config/0
  , repo_2_model_config/0
  , sign_fields/0
  , model_2_repo_config/0
  , model_2_model_config/0
  , model_2_model_list_config/0
]).
%%-------------------------------------------------------------------
-define(TXN, ?MODULE).

-record(?TXN, {
  mcht_id = 9999 :: mcht_id()
  , mcht_txn_date = <<>> :: binary()
  , mcht_txn_seq = <<"9999">> :: mcht_txn_seq()
  , query_id
  , settle_date :: binary()
  , quota = 0 :: non_neg_integer()
  , resp_code :: mcht_resp_code()
  , resp_msg :: mcht_resp_msg()
  , mcht_front_url
  , mcht_back_url
  , signature :: mcht_signature()
  , txn_status
  , mcht_index_key :: tuple()
}).

-type ?TXN() :: #?TXN{}.
-export_type([?TXN/0]).
-export_records([?TXN]).

%%-------------------------------------------------------------------
model_type() ->
  mcht.

out_2_model_config() ->
  [
  ].

model_2_out_config() ->
  [
  ].

repo_2_model_config() ->
  [

  ].

sign_fields() ->
  [
    mcht_id
    , mcht_txn_date
    , mcht_txn_seq
    , query_id
    , settle_date
    , quota
    , resp_code
    , resp_msg

  ].

model_2_repo_config() ->
  [
    {query_id, query_id}
    , {settle_date, settle_date}
    , {quota, quota}
    , {resp_code, resp_code}
    , {resp_msg, resp_msg}
    , {txn_status, txn_status}

  ].

%% UpRespPay -> MchtRespPay
model_2_model_config() ->
  [
    {mcht_id, {element, 1, mcht_index_key}}
    , {mcht_txn_date, {element, 2, mcht_index_key}}
    , {mcht_txn_seq, {element, 3, mcht_index_key}}
    , {query_id, up_orderId}
%%    , {settle_date, fun settle_date/2, [up_settleDate, mcht_index_key]}
    , {settle_date, up_settleDate}
    , {resp_code, up_respCode}
    , {resp_msg, up_respMsg}
    , {txn_status, fun xfutils:up_resp_code_2_txn_status/1, [up_respCode]}
    , {mcht_front_url, fun get_mcht_front_url/1, [mcht_index_key]}
    , {mcht_back_url, fun get_mcht_back_url/1, [mcht_index_key]}
    , {mcht_index_key, mcht_index_key}
  ].

model_2_model_list_config() ->
  [
    {default,
      [
        {repo_up_txn_log_pt, protocol_mcht_resp_pay,
          [
            {mcht_id, {element, 1, mcht_index_key}}
            , {mcht_txn_date, {element, 2, mcht_index_key}}
            , {mcht_txn_seq, {element, 3, mcht_index_key}}
            , {query_id, up_orderId}
            , {settle_date, up_settleDate}
            , {resp_code, up_respCode}
            , {resp_msg, up_respMsg}
            , {txn_status, fun xfutils:up_resp_code_2_txn_status/1, [up_respCode]}
            , {mcht_front_url, fun get_mcht_front_url/1, [mcht_index_key]}
            , {mcht_back_url, fun get_mcht_back_url/1, [mcht_index_key]}
            , {mcht_index_key, mcht_index_key}
          ]
        }
      ]
    }
  ].
%%-------------------------------------------------------------------
settle_date(SettleDate, {_, MchtTxnDate, _}) ->
  xfutils:prefix_yyyy_2_settle_date(SettleDate, MchtTxnDate).

get_txn_status(<<"00">>) ->
  success;
get_txn_status(_) ->
  fail.

get_mcht_front_url(MchtIndexKey) when is_tuple(MchtIndexKey) ->
  {ok, [RepoMcht]} = behaviour_repo:fetch(repo_mcht_txn_log_pt, MchtIndexKey),
  utils_recop:get(repo_mcht_txn_log_pt, RepoMcht, mcht_front_url).

get_mcht_back_url(MchtIndexKey) when is_tuple(MchtIndexKey) ->
  {ok, [RepoMcht]} = behaviour_repo:fetch(repo_mcht_txn_log_pt, MchtIndexKey),
  utils_recop:get(repo_mcht_txn_log_pt, RepoMcht, mcht_back_url).

