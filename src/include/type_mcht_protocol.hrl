%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2016 16:17
%%%-------------------------------------------------------------------
-author("simonxu").

-type request_qs() :: proplists:proplist().
-type date() :: {Year :: 1900..2100, Month :: 1..12, Day :: 1..31}.
-type time() :: {Hour :: 0..23, Min :: 0..59, Second :: 0..59}.
-type mcht_id() :: byte6().
-type mcht_txn_date() :: byte8().
-type mcht_txn_time() :: byte6().
-type mcht_txn_seq() :: byte23_up().
-type mcht_order_seq() :: bytes().
-type mcht_txn_amt() :: non_neg_integer().
-type mcht_order_desc() :: bytes().
-type gateway_id() :: byte4().
-type mcht_signature() :: bytes().
-type prod_id() :: bytes().
-type prod_bank_acct_id() :: bytes().
-type prod_bank_acct_corp_name() :: bytes().
-type prod_bank_name() :: bytes().
-type mcht_resp_code() :: byte2().
-type mcht_resp_msg() :: bytes().
-type mcht_front_url() :: bytes().
-type mcht_back_url() :: bytes().
-type prod_memo() :: bytes().
-type settle_date() :: byte4().
-type txn_status() :: success | waiting |fail.


-type query_id() :: byte23_up().

-export_type([mcht_id/0, mcht_txn_date/0, mcht_txn_time/0]).