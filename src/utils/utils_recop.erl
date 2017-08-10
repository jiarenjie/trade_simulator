%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2017 10:21 AM
%%%-------------------------------------------------------------------
-module(utils_recop).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([
  table_name/1
  , fields/1
  , new/2
  , new_empty/1
  , get/3
  , get/4
  , set/3
  , set/4
  , inc/3
  , inc/4
  , from_map/2
  , from_model/2
  , to_model/2
  , to_map/2
  , to_proplists/2
  , pr/2
  , lager/3
]).

-compile(export_all).
%% exprecs interface
table_name(M) when is_atom(M) ->
  [TableName] = apply(M, '#exported_records-', []),
  TableName.

fields(M) when is_atom(M) ->
  TableName = table_name(M),
  Fields = apply(M, '#info-', [TableName, fields]),
  Fields.

new_empty(M) when is_atom(M) ->
  TableName = table_name(M),
  apply(M, '#new-', [TableName]).

new(M, List) when is_atom(M), is_list(List) ->
  EmptyRec = new_empty(M),
  NewRec = apply(M, '#set-', [List, EmptyRec]),
  NewRec;

new(M, Map) when is_atom(M), is_map(Map) ->
  List = maps:to_list(Map),
  new(M, List).
%%-------------------------------------------------------------------
%% getter/setter
get(M, Repo, Key, Default) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  case get(M, Repo, Key) of
    undefined -> Default;
    Value -> Value
  end.

get(M, Model, up_index_key)
  when (M =:= protocol_up_req_pay)
  or (M =:= protocol_up_resp_pay)
  or (M =:= protocol_up_resp_query)
  ->
  VL = get(M, Model, [merId, txnTime, orderId]),
  list_to_tuple(VL);
get(M, Model, mcht_index_key)
  when (M =:= protocol_mcht_req_pay)
  or (M =:= protocol_mcht_req_query)
  or (M =:= protocol_mcht_req_refund)
  or (M =:= protocol_mcht_resp_refund)
  or (M =:= protocol_mcht_notify_refund)
  ->
  VL = get(M, Model, [mcht_id, mcht_txn_date, mcht_txn_seq]),
  list_to_tuple(VL);
get(M, Model, orig_mcht_index_key)
  when (M =:= protocol_mcht_req_refund)
  or (M =:= protocol_mcht_req_query)
  or (M =:= repo_mcht_txn_log_pt)
  ->
  VL = get(M, Model, [mcht_id, orig_mcht_txn_date, orig_mcht_txn_seq]),
  list_to_tuple(VL);
get(M, Repo, Key) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  Value = apply(M, '#get-', [Key, Repo]),
  Value;
get(M, Repo, Keys) when is_atom(M), is_tuple(Repo), is_list(Keys) ->
%%  Values = apply(M, '#get-', [Keys, Repo]),
%%  Values.
  [get(M, Repo, Key) || Key <- Keys].

get_test() ->
  Model = {protocol_up_resp_pay, <<"5.0.0">>, <<"UTF-8">>, <<"69597475696">>, <<"FZWH/OrUrEeqIF9b0zePwP4CAlk72rfe5QYZOuDCJVpWdmg3gHYWKPsOTwfBszy/R3uPqp9at6MzzwNIzFzIadHkMCQuyJ2rNyjzQc31IuxM7Ch+1AO0+CAArM8vx2GOmNEprhIw04YFAYMParmMJ/KMBROuD/k1BwsyHipEWm2N8sbYpIf4eVG7TM96rRsbmMCrr4WCJ+LeRATNUlLEZMCMowBsk92ZcjN3H+yN/ow3DGSQdBNNsidW3bhFZ/Celc9TT1XoChP8LH7Mg9K9hcifgxmEQxExJOhnyixOYvXnE1bSCfREfO0D3/vaGDyOSfx9SIUIzBm36Fyj8uQ6jA==">>, <<"01">>, <<"01">>, <<"01">>, <<"000201">>, <<"0">>, <<"898350249000242">>, <<"20170122183107665850290">>, <<"20170122183107">>, 100, <<"156">>, <<123, 112, 73, 61, 116, 101, 115, 116, 44, 97, 73, 61, 48, 51, 52, 50, 57, 53, 48, 48, 48, 52, 48, 48, 48, 54, 50, 49, 50, 44, 97, 78, 61, 228, 184, 138, 230, 181, 183, 232, 129, 154, 229, 173, 154, 233, 135, 145, 232, 158, 141, 228, 191, 161, 230, 129, 175, 230, 156, 141, 229, 138, 161, 230, 156, 137, 233, 153, 144, 229, 133, 172, 229, 143, 184, 44, 97, 66, 61, 229, 134, 156, 228, 184, 154, 233, 147, 182, 232, 161, 140, 228, 184, 138, 230, 181, 183, 229, 188, 160, 230, 177, 159, 233, 155, 134, 231, 148, 181, 230, 184, 175, 230, 148, 175, 232, 161, 140, 125>>, <<>>, <<>>, <<"201701221831073770348">>, <<"00">>, <<"Success!">>, 100, <<"156">>, <<"0122">>, <<"377034">>, <<"0122183107">>},
  IndexKey = get(protocol_up_resp_pay, Model, up_index_key),
  MerId = get(protocol_up_resp_pay, Model, merId),
  TxnTime = get(protocol_up_resp_pay, Model, txnTime),
  OrderId = get(protocol_up_resp_pay, Model, orderId),
  ?assertEqual(IndexKey, {MerId, TxnTime, OrderId}).
%%-------------------------------------------------------------------
set(_M, _Repo, id, _Value) ->
  {error, pk_could_not_be_changed};
set(M, Repo, Key, Value) when is_atom(M), is_tuple(Repo), is_atom(Key) ->
  ValueList = [{Key, Value}],
  set(M, Repo, ValueList).

set(M, Repo, ValueLists) when is_atom(M), is_tuple(Repo), is_list(ValueLists) ->
  %%TableName = table_name(M),
  RepoNew = apply(M, '#set-', [ValueLists, Repo]),
  RepoNew.
%%-------------------------------------------------------------------
inc(_M, _Repo, id, _Value) ->
  {error, pk_could_not_be_changed};
inc(M, Repo, Key, IncValue) when is_atom(M), is_tuple(Repo), is_atom(Key), is_integer(IncValue) ->
  OldValue = get(M, Repo, Key),
  ValueList = [{Key, OldValue + IncValue}],
  RepoNew = apply(M, '#set-', [ValueList, Repo]),
  RepoNew.

inc(M, Repo, {Key, IncValue}) when is_atom(M), is_tuple(Repo), is_integer(IncValue), is_atom(Key) ->
  inc(M, Repo, Key, IncValue).

%%-------------------------------------------------------------------
%% model (map) <==> repo(record)
to_proplists(M, Repo) when is_atom(M), is_tuple(Repo) ->
  Fields = fields(M),
  ValueList = tl(tuple_to_list(Repo)),
%%  lager:info("Fields = ~p,ValueList = ~p", [Fields, ValueList]),
  Ret = lists:zip(Fields, ValueList),
%%  lager:info("Ret = ~p", [Ret]),
  Ret.

to_proplists_test() ->
  Model = {protocol_mcht_resp_pay, <<"00001">>, <<"20170206">>,
    <<"20170206220217263071559">>, <<"20170206220219884577370">>,
    <<>>, 0, <<"05">>,
    <<228, 186, 164, 230, 152, 147, 229, 183, 178, 229, 143, 151, 231, 144,
      134, 239, 188, 140, 232, 175, 183, 231, 168, 141, 229, 144, 142, 230,
      159, 165, 232, 175, 162, 228, 186, 164, 230, 152, 147, 231, 187, 147,
      230, 158, 156, 91, 54, 49, 53, 52, 48, 51, 55, 93>>,
    <<"http://localhost:8888/pg/simu_mcht_front_succ">>,
    <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>,
    <<"2014FAFBFC47128A0F52E9F894516402D6B3DBAEE25A2E5BC9B099048D25143D7D132E29BE9D61544D1DBB215EF6D9A7E15826EF26A4FCC285508BB218AD55432FCEECA6A7B870ACED6E3FD4BED74084C0FECE2AECE9142D867EC4B29755BAEBD850CAA2AD942C078426CBE3563586C49DE55F9D76D1ABC53F744888B122E2832A73E57F48D54933D2A24132FF3C04884C518CADEC1B6FC8252A60C38DA08D8D9CCF156E6CAEF36E01EB412D8A01A5D13CFC7F7420A05BF6426216C50B6435EC8BA70B62789C523A70DE2B801344ECA3078E69E1DC41908CD2EADC76A679DA653E2F7194DAEA21799EC206D063EEB09D9FEC4973FB539E287834700A88E33677">>,
    waiting,
    {<<"00001">>, <<"20170206">>, <<"20170206220217263071559">>}},

  Props = [{mcht_id, <<"00001">>},
    {mcht_txn_date, <<"20170206">>},
    {mcht_txn_seq, <<"20170206220217263071559">>},
    {query_id, <<"20170206220219884577370">>},
    {settle_date, <<>>},
    {quota, 0},
    {resp_code, <<"05">>},
    {resp_msg, <<228, 186, 164, 230, 152, 147, 229, 183, 178, 229, 143,
      151, 231, 144, 134, 239, 188, 140, 232, 175, 183, 231,
      168, 141, 229, 144, 142, 230, 159, 165, 232, 175, 162,
      228, 186, 164, 230, 152, 147, 231, 187, 147, 230, 158,
      156, 91, 54, 49, 53, 52, 48, 51, 55, 93>>},
    {mcht_front_url, <<"http://localhost:8888/pg/simu_mcht_front_succ">>},
    {mcht_back_url, <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>},
    {signature, <<"2014FAFBFC47128A0F52E9F894516402D6B3DBAEE25A2E5BC9B099048D25143D7D132E29BE9D61544D1DBB215EF6D9A7E15826EF26A4FCC285508BB218AD55432FCEECA6A7B870ACED6E3FD4BED74084C0FECE2AECE9142D867EC4B29755BAEBD850CAA2AD942C078426CBE3563586C49DE55F9D76D1ABC53F744888B122E2832A73E57F48D54933D2A24132FF3C04884C518CADEC1B6FC8252A60C38DA08D8D9CCF156E6CAEF36E01EB412D8A01A5D13CFC7F7420A05BF6426216C50B6435EC8BA70B62789C523A70DE2B801344ECA3078E69E1DC41908CD2EADC76A679DA653E2F7194DAEA21799EC206D063EEB09D9FEC4973FB539E287834700A88E33677">>},
    {txn_status, waiting},
    {mcht_index_key, {<<"00001">>, <<"20170206">>,
      <<"20170206220217263071559">>}}],
  ?assertEqual(Props, to_proplists(protocol_mcht_resp_pay, Model)),
  ok.

to_map(M, Repo) when is_atom(M), is_tuple(Repo) ->
  List = to_proplists(M, Repo),
  maps:from_list(List).

to_model(M, List) when is_atom(M), is_list(List) ->
  [to_model(M, Repo) || Repo <- List];
to_model(M, Repo) when is_atom(M), is_tuple(Repo) ->
  to_map(M, Repo).

from_model(M, Model) when is_atom(M), is_map(Model) ->
  from_map(M, Model).

from_map(M, Model) when is_atom(M), is_map(Model) ->
  List = maps:to_list(Model),
  %%TableName = table_name(M),
  EmptyR = new_empty(M),
  apply(M, '#fromlist-', [List, EmptyR]).
%%-------------------------------------------------------------------
pr(M, Repo) when is_atom(M), is_tuple(Repo) ->
%%  lager:info("M = ~p,Repo= ~p", [M, Repo]),
  VL = to_proplists(M, Repo),
%%  lager:info("VL = ~p", [VL]),
  L = [pr_field(Field, Value) || {Field, Value} <- VL],
  lists:flatten(L);
pr(M, Model) when is_atom(M), is_map(Model) ->
  L = [pr_field(Field, maps:get(Field, Model)) || Field <- maps:keys(Model)],
%%  lager:info("xxx ==Model = ~p", [Model]),
%%  lager:info("L = ~p", [L]),
  lists:flatten(L).

pr_field(Field, Value) when
  ((Field =:= mcht_order_desc)
    or (Field =:= mcht_resp_msg)
    or (Field =:= resp_msg)
    or (Field =:= reqReserved)
    or (Field =:= signature)
    or (Field =:= reserved)
    or (Field =:= up_respMsg)
    or (Field =:= up_orderDesc)
    or (Field =:= up_reqReserved)
    or (Field =:= prod_bank_acct_corp_name)
    or (Field =:= prod_bank_name)
  ) ->
%%  lager:info("Field = ~p,Value = ~p", [Field, Value]),
  io_lib:format("~p=~ts,", [Field, Value]);
pr_field(Field, Value) ->
  io_lib:format("~p=~p,", [Field, Value]).

pr_test() ->
  Repo = #{
    mcht_index_key => {<<"00001">>, <<"20170123">>, <<"20170123143351679092532">>},
    txn_type => pay, up_merId => <<"898319849000018">>,
    up_txnTime => <<"20170123143400">>,
    up_orderId => <<"20170123143400069618399">>, up_txnAmt => 100,
    up_reqReserved => <<"{pI=test,aI=03429500040006212,aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行}"/utf8>>,
    up_orderDesc => <<"{pI=test,aI=03429500040006212,aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行}"/utf8>>,
    up_issInsCode => <<>>,
    up_index_key => {<<"898319849000018">>, <<"20170123143400">>, <<"20170123143400069618399">>},
    up_queryId => undefined, up_respCode => undefined,
    up_respMsg => undefined, up_settleAmt => undefined,
    up_settleDate => undefined, up_traceNo => undefined,
    up_traceTime => undefined, up_query_index_key => undefined,
    txn_status => waiting, up_accNo => undefined
  },

  Out = pr(repo_up_txn_log_pt, Repo),
  OutTrim = trim_pretty(Out),
  Expected = <<"mcht_index_key={<<\"00001\">>,<<\"20170123\">>,<<\"20170123143351679092532\">>},txn_status=waiting,txn_type=pay,"
  "up_accNo=undefined,up_index_key={<<\"898319849000018\">>,<<\"20170123143400\">>,<<\"20170123143400069618399\">>},"
  "up_issInsCode=<<>>,up_merId=<<\"898319849000018\">>,up_orderDesc={pI=test,aI=03429500040006212,aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行},"
  "up_orderId=<<\"20170123143400069618399\">>,"
  "up_queryId=undefined,up_query_index_key=undefined,up_reqReserved={pI=test,aI=03429500040006212,aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行},"
  "up_respCode=undefined,up_respMsg=undefined,up_settleAmt=undefined,up_settleDate=undefined,up_traceNo=undefined,up_traceTime=undefined,"
  "up_txnAmt=100,up_txnTime=<<\"20170123143400\">>,"/utf8>>,

%%  lager:error("~p", [Out]),
%%  lager:error("~p", [Expected]),
  [E] = io_lib:format("~ts", [Expected]),
  ?assertEqual(OutTrim, E),

%%  ?assertEqual(
%%    binary_to_list(<<"mcht_id=<<\"00001\">>,mcht_txn_date=<<\"20170206\">>,mcht_txn_seq=<<\"20170206220217263071559\">>,"
%%    "query_id=<<\"20170206220219884577370\">>,settle_date=<<>>,quota=0,resp_code=<<\"05\">>,"
%%    "resp_msg=交易已受理，请稍后查询交易结果[6154037],mcht_front_url=<<\"http://localhost:8888/pg/simu_mcht_front_succ\">>,"
%%    "mcht_back_url=<<\"http://localhost:8888/pg/simu_mcht_back_succ_info\">>,"
%%    "signature=2014FAFBFC47128A0F52E9F894516402D6B3DBAEE25A2E5BC9B099048D25143D7D132E29BE9D61544D1DBB215EF6D9A7E15826EF26A4FCC285508BB218AD55432FCEECA6A7B870ACED6E3FD4BED74084C0FECE2AECE9142D867EC4B29755BAEBD850CAA2AD942C078426CBE3563586C49DE55F9D76D1ABC53F744888B122E2832A73E57F48D54933D2A24132FF3C04884C518CADEC1B6FC8252A60C38DA08D8D9CCF156E6CAEF36E01EB412D8A01A5D13CFC7F7420A05BF6426216C50B6435EC8BA70B62789C523A70DE2B801344ECA3078E69E1DC41908CD2EADC76A679DA653E2F7194DAEA21799EC206D063EEB09D9FEC4973FB539E287834700A88E33677,"
%%    "txn_status=waiting,mcht_index_key={<<\"00001\">>,<<\"20170206\">>,<<\"20170206220217263071559\">>},"/utf8>>),
%%    pr(protocol_mcht_resp_pay,
%%      {protocol_mcht_resp_pay, <<"00001">>, <<"20170206">>,
%%        <<"20170206220217263071559">>, <<"20170206220219884577370">>,
%%        <<>>, 0, <<"05">>,
%%        <<228, 186, 164, 230, 152, 147, 229, 183, 178, 229, 143, 151, 231, 144,
%%          134, 239, 188, 140, 232, 175, 183, 231, 168, 141, 229, 144, 142, 230,
%%          159, 165, 232, 175, 162, 228, 186, 164, 230, 152, 147, 231, 187, 147,
%%          230, 158, 156, 91, 54, 49, 53, 52, 48, 51, 55, 93>>,
%%        <<"http://localhost:8888/pg/simu_mcht_front_succ">>,
%%        <<"http://localhost:8888/pg/simu_mcht_back_succ_info">>,
%%        <<"2014FAFBFC47128A0F52E9F894516402D6B3DBAEE25A2E5BC9B099048D25143D7D132E29BE9D61544D1DBB215EF6D9A7E15826EF26A4FCC285508BB218AD55432FCEECA6A7B870ACED6E3FD4BED74084C0FECE2AECE9142D867EC4B29755BAEBD850CAA2AD942C078426CBE3563586C49DE55F9D76D1ABC53F744888B122E2832A73E57F48D54933D2A24132FF3C04884C518CADEC1B6FC8252A60C38DA08D8D9CCF156E6CAEF36E01EB412D8A01A5D13CFC7F7420A05BF6426216C50B6435EC8BA70B62789C523A70DE2B801344ECA3078E69E1DC41908CD2EADC76A679DA653E2F7194DAEA21799EC206D063EEB09D9FEC4973FB539E287834700A88E33677">>,
%%        waiting,
%%        {<<"00001">>, <<"20170206">>, <<"20170206220217263071559">>}}
%%    )
%%  ),
  ok.

trim_pretty(L) ->
  F = fun(Char, AccIn) when (Char =:= 32) or (Char =:= 10) ->
    AccIn;
    (Char, AccIn) ->
      [Char | AccIn]
      end,

  RL = lists:foldl(F, [], L),
  lists:reverse(RL).


%%-------------------------------------------------------------------
to_post(M, Repo, string) ->
  PL = to_proplists(M, Repo),
  xfutils:post_vals_to_iolist(PL).

%%-------------------------------------------------------------------
lager(Level, M, Model) ->
  String = pr(M, Model),
  lager_out(Level, M, String).

lager_out(debug, M, String) ->
  lager:debug("~p = ~ts", [M, String]);
lager_out(info, M, String) ->
  lager:info("~p = ~ts", [M, String]);
lager_out(error, M, String) ->
  lager:error("~p = ~ts", [M, String]).

