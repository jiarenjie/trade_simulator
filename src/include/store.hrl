%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc 存放和mnesia长期存储有关的结构/表定义
%%%
%%% @end
%%% Created : 08. Apr 2016 16:10
%%%-------------------------------------------------------------------
-author("simonxu").

-define(TXNLOG, txn_log).
-define(MCHTINFO, mcht_info).

-record(txn_log, {
  mcht_txn_seq
  , mcht_txn_date
  , mcht_txn_time
  , up_orderId
  , up_txnAmt
  , txn_statue
  , up_respCode
}).

-type up_txn_log() :: #txn_log{}.
-export_type([up_txn_log/0]).



