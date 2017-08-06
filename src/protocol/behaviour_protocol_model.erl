%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2017 9:49 PM
%%%-------------------------------------------------------------------
-module(behaviour_protocol_model).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

-type model_type() :: mcht | up | ums.
-export_type([model_type/0]).

%% callbacks
-callback model_type() -> model_type().
-callback out_2_model_config() -> list().
-callback model_2_out_config() -> list().
-callback repo_2_model_config() -> list().
-callback model_2_repo_config() -> list().

-callback model_2_model_list_config() -> list().

-callback sign_fields() -> list().

%% API
-export([
%%  model_2_out_map/1
  sign/1
  , out_2_model/2
  , model_2_out_post/2
  , model_2_out/3
  , repo_2_model/3
%%  , model_2_model/3
  , model_2_model_list/2
  , model_2_model_list/3
  , verify/3
  , save/2
  , fetch/2
  , fetch_index/2

  , mcht_sign_msg/3
]).

-compile(export_all).

-type protocol_models() :: protocol_up_req_pay:protocol_up_req_pay()
| protocol_up_resp_pay:protocol_up_resp_pay()
| protocol_mcht_req_pay:protocol_mcht_req_pay().
-type protocol_modules() :: protocol_up_req_pay | protocol_up_resp_pay | protocol_up_req_query | protocol_up_resp_query
| protocol_mcht_req_pay | protocol_mcht_resp_pay | protocol_mcht_req_query | protocol_mcht_resp_query
| protocol_mcht_req_refund | protocol_mcht_resp_refund.

%%-------------------------------------------------------------------
model_2_out_map(mcht) ->
  #{
    mcht_id => <<"merchId">>
    , mcht_txn_date => <<"tranDate">>
    , mcht_txn_seq => <<"tranId">>
    , mcht_txn_time => <<"tranTime">>
    , mcht_txn_amt => {<<"tranAmt">>, integer}
    , mcht_order_desc=> <<"orderDesc">>
    , gateway_id=> <<"gateWayId">>
    , signature=> <<"signature">>
    , prod_id=> <<"prodId">>
    , prod_memo=> <<"prodMemo">>
    , prod_bank_acct_id=> <<"accountId">>
    , prod_bank_acct_corp_name=> <<"accountName">>
    , prod_bank_name=> <<"accountBank">>
    , mcht_front_url=> <<"trustFrontUrl">>
    , mcht_back_url=> <<"trustBackUrl">>
    , bank_card_no=> <<"bankCardNo">>
    , bank_id=> <<"bankId">>
    , mcht_index_key=> {[<<"merchId">>, <<"tranDate">>, <<"tranId">>], tuple}

    , orig_mcht_txn_date => <<"origTranDate">>
    , orig_mcht_txn_seq => <<"origTranId">>
    , orig_query_id => <<"origQueryId">>
    , orig_resp_code => <<"origRespCode">>
    , orig_resp_msg => <<"origRespMsg">>
    , query_id => <<"queryId">>
    , settle_date => <<"settleDate">>
    , resp_code => <<"respCode">>
    , resp_msg => <<"respMsg">>
    , quota => {<<"quota">>, integer}

  };

%%%-------------------------------------------------------------------
model_2_out_map(up) ->
  #{

    version => <<"version">>
    , encoding=> <<"encoding">>
    , certId=> <<"certId">>
    , signature=> <<"signature">>
    , signMethod=> <<"signMethod">>
    , txnType=> <<"txnType">>
    , txnSubType=> <<"txnSubType">>
    , bizType=> <<"bizType">>
    , channelType=> <<"channelType">>
    , accessType=> <<"accessType">>
    , merId=> <<"merId">>
    , txnTime=> <<"txnTime">>
    , orderId=> <<"orderId">>
    , queryId=> <<"queryId">>

    , txnAmt => {<<"txnAmt">>, integer}
    , currencyCode =><<"currencyCode">>
    , reqReserved =><<"reqReserved">>
    , respCode =><<"respCode">>
    , respMsg =><<"respMsg">>
    , settleAmt =>{<<"settleAmt">>, integer}
    , settleCurrencyCode =><<"settleCurrencyCode">>
    , settleDate =><<"settleDate">>
    , traceNo =><<"traceNo">>
    , traceTime =><<"traceTime">>
    , txnSubType =><<"txnSubType">>
    , txnTime =><<"txnTime">>
    , reserved =><<"reserved">>
    , accNo =><<"accNo">>

    , origRespCode => <<"origRespCode">>
    , origRespMsg => <<"origRespMsg">>
    , issuerIdentifyMode => <<"issuerIdentifyMode">>
  }.

%%%-------------------------------------------------------------------

get_model_type(M) when is_atom(M) ->
  apply(M, model_type, []).
%%get_model_type(Model) when is_tuple(Model) ->
%%  M = element(1, Model),
%%  get_model_type(M).

get_model_type_test() ->
  ?assertEqual(get_model_type(protocol_mcht_req_pay), mcht),
  ?assertEqual(get_model_type(protocol_up_req_pay), up),
  ok.


get_model_name(Model) when is_tuple(Model) ->
  element(1, Model).


%%%-------------------------------------------------------------------
sign(Model) when is_tuple(Model) ->
  M = get_model_name(Model),
%%  ModelType = apply(M, model_type, []),
  ModelType = get_model_type(M),
  Sign = sign(ModelType, M, Model),
  ModelWithSig = utils_recop:set(M, Model, [{signature, Sign}]),
  ModelWithSig.

sign(up, MProtocol, Repo) ->
  SignFields = apply(MProtocol, sign_fields, []),
  up_sign_msg(MProtocol, Repo, SignFields);
sign(mcht, MProtocol, Repo) ->
  SignFields = apply(MProtocol, sign_fields, []),
  {_, Sig} = mcht_sign_msg(MProtocol, Repo, SignFields),
  Sig.


%%-------------------------------------------------------------------

get_model_field_out_config(M, Field) ->
  ModelType = apply(M, model_type, []),
  ConfigMap = model_2_out_map(ModelType),
  FieldConfig = maps:get(Field, ConfigMap),
  FieldConfig.

get_model_field_out_config_test() ->
  Config = get_model_field_out_config(protocol_mcht_req_pay, mcht_txn_amt),
  ?assertEqual(Config, {<<"tranAmt">>, integer}),

  ?assertEqual(get_model_field_out_config(protocol_mcht_req_pay, mcht_index_key),
    {[<<"merchId">>, <<"tranDate">>, <<"tranId">>], tuple}),
  ok.

model_2_out_one_field(M, Model, Field) when is_atom(M), is_tuple(Model), is_atom(Field) ->
  Config = get_model_field_out_config(M, Field),
  do_model_2_out_one_field(M, Model, {Field, Config}).

do_model_2_out_one_field(M, Model, {Field, FieldInPVBIN}) when is_atom(Field), is_binary(FieldInPVBIN) ->
  Value = utils_recop:get(M, Model, Field),
  {FieldInPVBIN, Value};
do_model_2_out_one_field(M, Model, {Field, {FieldInPVBIN, integer}}) when is_atom(Field), is_binary(FieldInPVBIN) ->
  ValueInteger = utils_recop:get(M, Model, Field),
  ValueBin = integer_to_binary(ValueInteger),
  {FieldInPVBIN, ValueBin}.

do_model_2_out_one_field_test() ->
  M = protocol_mcht_req_pay,
  Model = utils_recop:new(M, [{mcht_txn_amt, 9997}, {mcht_txn_date, <<"20101010">>}, {mcht_txn_seq, <<"seqqq">>}]),
  ?assertEqual(do_model_2_out_one_field(M, Model, {mcht_txn_amt, {<<"tranAmt">>, integer}}),
    {<<"tranAmt">>, <<"9997">>}),
  ?assertEqual(do_model_2_out_one_field(M, Model, {mcht_txn_date, <<"tranDate">>}),
    {<<"tranDate">>, <<"20101010">>}),
  ?assertEqual(do_model_2_out_one_field(M, Model, {mcht_txn_seq, <<"tranId">>}),
    {<<"tranId">>, <<"seqqq">>}),

  ?assertEqual(model_2_out_one_field(M, Model, mcht_txn_amt),
    {<<"tranAmt">>, <<"9997">>}),
  ?assertEqual(model_2_out_one_field(M, Model, mcht_txn_date),
    {<<"tranDate">>, <<"20101010">>}),
  ?assertEqual(model_2_out_one_field(M, Model, mcht_txn_seq),
    {<<"tranId">>, <<"seqqq">>}),
  ok.

%% output model via OutputFields
do_model_2_out_post(M, Model, OutputFields) when is_atom(M), is_tuple(Model), is_list(OutputFields) ->

  [model_2_out_one_field(M, Model, Field4Out) || Field4Out <- OutputFields].

do_model_2_out_post_test() ->
  M = protocol_mcht_req_pay,
  Model = utils_recop:new(M, [{mcht_txn_amt, 9997}, {mcht_txn_date, <<"20101010">>}, {mcht_txn_seq, <<"seqqq">>}]),
  ?assertEqual(do_model_2_out_post(M, Model, [mcht_txn_amt, mcht_txn_date, mcht_txn_seq]),
    [
      {<<"tranAmt">>, <<"9997">>}
      , {<<"tranDate">>, <<"20101010">>}
      , {<<"tranId">>, <<"seqqq">>}
    ]
  ),
  ok.


-spec model_2_out_post(M, Model) -> PV when
  M :: protocol_modules(),
  Model :: protocol_models(),
  PV :: proplists:proplist().

model_2_out_post(M, Model) when is_atom(M), is_tuple(Model) ->
  OutFields = get_out_fields(M),
  do_model_2_out_post(M, Model, OutFields).

model_2_out_post_test() ->
  Model = utils_recop:new(protocol_mcht_resp_pay,
    [
      {mcht_id, 1}
      , {mcht_txn_date, <<"20170909">>}
      , {mcht_txn_seq, <<"7777">>}
      , {query_id, <<"qqq">>}
      , {settle_date, <<"20171010">>}
      , {quota, 0}
      , {resp_code, <<"00">>}
      , {resp_msg, <<"success!">>}
      , {mcht_front_url, <<"http://front_url">>}
      , {mcht_back_url, <<"http://back_url">>}
      , {signature, <<"mchtsignature">>}
      , {txn_status, success}
    ]),
  VL = model_2_out_post(protocol_mcht_resp_pay, Model),
  ?assertEqual(VL,
    [
      {<<"signature">>, <<"mchtsignature">>}
      , {<<"merchId">>, 1}
      , {<<"tranDate">>, <<"20170909">>}
      , {<<"tranId">>, <<"7777">>}
      , {<<"queryId">>, <<"qqq">>}
      , {<<"settleDate">>, <<"20171010">>}
      , {<<"quota">>, <<"0">>}
      , {<<"respCode">>, <<"00">>}
      , {<<"respMsg">>, <<"success!">>}

    ]),

  ok.
%%-------------------------------------------------------------------
model_2_out(M, Model, proplist) ->
  VL = model_2_out_post(M, Model),
  VL;
model_2_out(M, Model, post_body_string) ->
  VL = model_2_out_post(M, Model),
  xfutils:post_vals_to_iolist(VL).

model_2_out_test() ->
  ModelEmpty = utils_recop:new_empty(protocol_up_req_query),
  Model = utils_recop:set(protocol_up_req_query, ModelEmpty,
    [{merId, <<"898319849000018">>}
      , {txnTime, <<"20170206160227">>}
      , {orderId, <<"20170206160227544228896">>}
    ]),

  ?assertEqual(<<"signature=9&accessType=0&bizType=000201&certId=9&channelType=07&encoding=UTF-8&merId=898319849000018&orderId=20170206160227544228896&signMethod=01&txnSubType=00&txnTime=20170206160227&txnType=00&version=5.0.0">>,
    list_to_binary(model_2_out(protocol_up_req_query, Model, post_body_string))),

  ok.

%%-------------------------------------------------------------------
get_out_fields(M) when is_atom(M) ->
  [signature | apply(M, sign_fields, [])].
%%-------------------------------------------------------------------

-spec out_2_model(M, PL) -> Model when
  M :: protocol_modules(),
  PL :: proplists:proplist(),
  Model :: protocol_models().

%% 用途：将外部数据转换为内部格式
%% 实现：M： 可以得到对应的字段列表
%%          然后获取转换配置,model_2_out_map(Type)
%%
out_2_model(M, PL) when is_atom(M), is_list(PL) ->
  Fields = get_out_fields(M),
  ModelType = get_model_type(M),
  Model2OutMap = model_2_out_map(ModelType),

  F = fun out_2_model_one_field/2,
  {VL, _, _} = lists:foldl(F, {[], Model2OutMap, PL}, Fields),
  utils_recop:new(M, VL).

out_2_model_one_field(Field, {Acc, Model2OutMap, PL}) when is_atom(Field), is_list(Acc), is_map(Model2OutMap) ->
  Config = maps:get(Field, Model2OutMap),

%%  lager:debug("Config=~p,Field=~p", [Config, Field]),

  Value = do_out_2_model_one_field(Config, PL),
  %% omit undefined key/value , which means not appear in PL
  case Value of
    undefined ->
      {Acc, Model2OutMap, PL};
    Value ->
      AccNew = [{Field, Value} | Acc],
      {AccNew, Model2OutMap, PL}
  end.

do_out_2_model_one_field({KeyInPLList, tuple}, PL) when is_list(KeyInPLList), is_list(PL) ->
  VL = [proplists:get_value(Key, PL) || Key <- KeyInPLList],
  list_to_tuple(VL);
do_out_2_model_one_field({KeyInPL, integer}, PL) when is_binary(KeyInPL), is_list(PL) ->
  Value = proplists:get_value(KeyInPL, PL),
  case Value of
    undefined ->
      undefined;
    Value ->
      binary_to_integer(Value)
  end;
do_out_2_model_one_field(KeyInPL, PL) when is_binary(KeyInPL), is_list(PL) ->
  proplists:get_value(KeyInPL, PL).

do_out_2_model_one_field_test() ->
  PL = [
    {<<"accessType">>, <<"0">>}
    , {<<"bizType">>, <<"000201">>}
    , {<<"certId">>, <<"9">>}
    , {<<"currencyCode">>, <<"156">>}
    , {<<"encoding">>, <<"UTF-8">>}
    , {<<"merId">>, <<"012345678901234">>}
    , {<<"orderId">>, <<"20161111">>}
    , {<<"queryId">>, <<"20162222">>}
    , {<<"reqReserved">>, <<"reqR">>}
    , {<<"respCode">>, <<"00">>}
    , {<<"respMsg">>, <<"success">>}
    , {<<"settleAmt">>, <<"100">>}
    , {<<"settleCurrencyCode">>, <<"156">>}
    , {<<"settleDate">>, <<"20163333">>}
    , {<<"signature">>, <<"sig">>}
    , {<<"signMethod">>, <<"01">>}
    , {<<"traceNo">>, <<"22222">>}
    , {<<"traceTime">>, <<"101010">>}
    , {<<"txnAmt">>, <<"100">>}
    , {<<"txnSubType">>, <<"01">>}
    , {<<"txnTime">>, <<"19991212121212">>}
    , {<<"txnType">>, <<"01">>}
    , {<<"version">>, <<"5.0.0">>}
    , {<<"reserved">>, <<"reserved">>}
    , {<<"accNo">>, <<"95555">>}
  ],
  ?assertEqual(do_out_2_model_one_field(<<"accessType">>, PL), <<"0">>),
  ?assertEqual(do_out_2_model_one_field({<<"accessType">>, integer}, PL), 0),
  ?assertEqual(do_out_2_model_one_field({[<<"accessType">>], tuple}, PL), {<<"0">>}),
  ?assertEqual(do_out_2_model_one_field({[<<"accessType">>, <<"bizType">>], tuple}, PL), {<<"0">>, <<"000201">>}),

  ok.


out_2_model_test() ->
  PL = [
    {<<"accessType">>, <<"0">>}
    , {<<"bizType">>, <<"000201">>}
    , {<<"certId">>, <<"9">>}
    , {<<"currencyCode">>, <<"156">>}
    , {<<"encoding">>, <<"UTF-8">>}
    , {<<"merId">>, <<"012345678901234">>}
    , {<<"orderId">>, <<"20161111">>}
    , {<<"queryId">>, <<"20162222">>}
    , {<<"reqReserved">>, <<"reqR">>}
    , {<<"respCode">>, <<"00">>}
    , {<<"respMsg">>, <<"success">>}
    , {<<"settleAmt">>, <<"100">>}
    , {<<"settleCurrencyCode">>, <<"156">>}
    , {<<"settleDate">>, <<"20163333">>}
    , {<<"signature">>, <<"sig">>}
    , {<<"signMethod">>, <<"01">>}
    , {<<"traceNo">>, <<"22222">>}
    , {<<"traceTime">>, <<"101010">>}
    , {<<"txnAmt">>, <<"100">>}
    , {<<"txnSubType">>, <<"01">>}
    , {<<"txnTime">>, <<"19991212121212">>}
    , {<<"txnType">>, <<"01">>}
    , {<<"version">>, <<"5.0.0">>}
    , {<<"reserved">>, <<"reserved">>}
    , {<<"accNo">>, <<"95555">>}
  ],

  R = [{binary_to_atom(KeyBin, utf8), Value} || {KeyBin, Value} <- PL],

  M = protocol_up_resp_pay,
  ModelUpRespPay = out_2_model(M, PL),

  ModelExpected = utils_recop:set(M, utils_recop:new(M, R), [{txnAmt, 100}, {settleAmt, 100}]),

  ?assertEqual(ModelUpRespPay, ModelExpected),
  ok.

out_2_model_mcht_test() ->
  PostVals = t_utils_ut:qs_mcht_req_pay_1(),


  ModelMchtReqPay = out_2_model(protocol_mcht_req_pay, PostVals),

  ?assertEqual(utils_recop:get(protocol_mcht_req_pay, ModelMchtReqPay, mcht_index_key),
    {<<"00001">>, <<"20170124">>, <<"20170124140404395762577">>}),
  ?assertEqual(utils_recop:get(protocol_mcht_req_pay, ModelMchtReqPay, txn_type), pay),
  ?assertEqual(utils_recop:get(protocol_mcht_req_pay, ModelMchtReqPay, txn_status), waiting),


  ok.
%%-------------------------------------------------------------------
repo_2_model(MRepo, MProtocol, Repo) ->
  Config = apply(MProtocol, repo_2_model_config, []),


  F = fun({KeyProtocol, KeyRepo, binary}, AccIn) ->
    ValueRepo = utils_recop:get(MRepo, Repo, KeyRepo),
    ValueBin = integer_to_binary(ValueRepo),
    [{KeyProtocol, ValueBin} | AccIn];
    ({KeyProtocol, KeyRepo}, AccIn) ->
      ValueRepo = utils_recop:get(MRepo, Repo, KeyRepo),
      [{KeyProtocol, ValueRepo} | AccIn]
      end,

  VL = lists:foldl(F, [], Config),
  utils_recop:new(MProtocol, VL).

repo_2_model_test() ->
  RepoEmpty = utils_recop:new_empty(repo_up_txn_log_pt),
  Repo = utils_recop:set(repo_up_txn_log_pt, RepoEmpty, [
    {up_merId, <<"788">>}
    , {up_txnTime, <<"111111">>}
    , {up_orderId, <<"orderid9999">>}
    , {up_txnAmt, 100}
    , {up_settleAmt, 100}
    , {up_settleDate, <<"1230">>}
    , {up_respCode, <<"00">>}
    , {up_respMsg, <<"success">>}
    , {up_reqReserved, <<"reqRes">>}
    , {up_accNo, <<"95555">>}
  ]),

  Model = repo_2_model(repo_up_txn_log_pt, protocol_up_resp_pay, Repo),
  ModelEmpty = utils_recop:new_empty(protocol_up_resp_pay),
  ?assertEqual(Model, utils_recop:set(protocol_up_resp_pay, ModelEmpty, [
    {merId, <<"788">>}
    , {txnTime, <<"111111">>}
    , {orderId, <<"orderid9999">>}
    , {txnAmt, <<"100">>}
    , {settleAmt, <<"100">>}
    , {settleDate, <<"1230">>}
    , {respCode, <<"00">>}
    , {respMsg, <<"success">>}
    , {reqReserved, <<"reqRes">>}
    , {accNo, <<"95555">>}
  ])),
  ok.

%%-------------------------------------------------------------------
sign_fields(M) when is_atom(M) ->
  SignFields = apply(M, sign_fields, []),
  SignFields.

verify(up, MProtocol, Model) ->
  SignFields = sign_fields(MProtocol),
  up_verify_msg(MProtocol, Model, SignFields);

verify(mcht, MProtocol, Model) ->
  SignFields = sign_fields(MProtocol),
  mcht_verify_msg(MProtocol, Model, SignFields).

verify_test() ->
  xfutils:app_env_init_for_test(),

  PropList = [
    {<<"accessType">>, <<"0">>}
    , {<<"bizType">>, <<"000201">>}
    , {<<"certId">>, <<"69597475696">>}
    , {<<"currencyCode">>, <<"156">>}
    , {<<"encoding">>, <<"UTF-8">>}
    , {<<"merId">>, <<"898350249000242">>}
    , {<<"orderId">>, <<"20160327104342497873951">>}
    , {<<"queryId">>, <<"201603271043427768798">>}
    , {<<"reqReserved">>, <<"{pI=test,aI=03429500040006212,aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行}"/utf8>>}
    , {<<"respCode">>, <<"00">>}
    , {<<"respMsg">>, <<"success">>}
    , {<<"settleAmt">>, <<"1">>}
    , {<<"settleCurrencyCode">>, <<"156">>}
    , {<<"settleDate">>, <<"0327">>}
    , {<<"signMethod">>, <<"01">>}
    , {<<"signature">>, <<"Fnq+/mZkP5EmgY5iSVdJdiGUdR3Wuk9ios/oF/G0o2YexAf9JkVOPggCG3nwJMs03fnViBd7RbTedpdv/amIPJh8Y39KLeLGI3uXIEu5JNzabOvdlYrDOCBaTq89klI2zSNpZAf8oNVQs0vhHQEsDFPe9YKn4npP5VG2oZBmh79sl4sH71IKmq+TrAj50SlYluPdgO8k506K3LZ5jUKVu7d3+W38tblibYbisO2nbYdhb0SMkELrLFJzzMYqVKwVH93RwcO9XmpAKkAqIn/OZrEyu1/pgCJIltFGTf1m9Z0eM0fTzKMbqWjAgqJyKD4AalXxCxGRy8bqooMAfPytrA==">>}
    , {<<"traceNo">>, <<"776879">>}
    , {<<"traceTime">>, <<"0327104342">>}
    , {<<"txnAmt">>, <<"1">>}
    , {<<"txnSubType">>, <<"01">>}
    , {<<"txnTime">>, <<"20160327104342">>}
    , {<<"txnType">>, <<"01">>}
    , {<<"version">>, <<"5.0.0">>}
  ],

  Repo = out_2_model(protocol_up_resp_pay, PropList),
  VerifyResult = verify(up, protocol_up_resp_pay, Repo),
  ?assertEqual(VerifyResult, ok),
  ok.


%%-------------------------------------------------------------------
-spec model_2_repo(MProtocol, Req) -> RepoNew when
  MProtocol :: protocol_modules(),
  Req :: protocol_models(),
  RepoNew :: repo_up_txn_log_pt: up_txn_log().

model_2_repo(MProtocol, Model) when is_atom(MProtocol), is_tuple(Model) ->
  ValueList = model_2_proplists(MProtocol, Model),
  UpIndexKey = utils_recop:get(MProtocol, Model, up_index_key),
  {ok, RepoNew} = behaviour_repo:update(repo_up_txn_log_pt, {up_index_key, UpIndexKey}, ValueList),
  RepoNew.

model_2_repo(up, MProtocol, Model) when is_atom(MProtocol), is_tuple(Model) ->
  ValueList = model_2_proplists(MProtocol, Model),
  UpIndexKey = utils_recop:get(MProtocol, Model, up_index_key),
%%  UpIndexKey = proplists:get_value(up_index_key, ValueList),
  {ok, RepoNew} = behaviour_repo:update(repo_up_txn_log_pt, {up_index_key, UpIndexKey}, ValueList),
  RepoNew.


model_2_repo_pk(up, MProtocol, Model) when is_atom(MProtocol), is_tuple(Model) ->
  ValueList = model_2_proplists(MProtocol, Model),
  MchtIndexKey = utils_recop:get(MProtocol, Model, mcht_index_key),
  {ok, RepoNew} = behaviour_repo:update_create_pk(repo_up_txn_log_pt, MchtIndexKey, ValueList),
  RepoNew;
model_2_repo_pk(mcht, MProtocol, Model) when is_atom(MProtocol), is_tuple(Model) ->
  ValueList = model_2_proplists(MProtocol, Model),
  MchtIndexKey = utils_recop:get(MProtocol, Model, mcht_index_key),
  {ok, RepoNew} = behaviour_repo:update_create_pk(repo_mcht_txn_log_pt, MchtIndexKey, ValueList),
  RepoNew.


model_2_proplists(MProtocol, Model) ->
  Config = apply(MProtocol, model_2_repo_config, []),
  ValueList = [get_one_key_value(MProtocol, Model, Item) || Item <- Config],
  ValueList.

get_one_key_value(_MProtocol, _Model, {RepoKey, {static, Value}}) ->
  {RepoKey, Value};
get_one_key_value(MProtocol, Model, {RepoKey, ModelKeyList, tuple}) ->
  L = [utils_recop:get(MProtocol, Model, KeyModel) || KeyModel <- ModelKeyList],
  {RepoKey, list_to_tuple(L)};
get_one_key_value(MProtocol, Model, {RepoKey, ModelKey, FConvert}) when is_atom(ModelKey), is_function(FConvert) ->
  Value = utils_recop:get(MProtocol, Model, ModelKey),
  ValueConverted = FConvert(Value),
  {RepoKey, ValueConverted};
get_one_key_value(MProtocol, Model, {RepoKey, ModelKeyList, FConvert}) when is_list(ModelKeyList), is_function(FConvert) ->
  ValueList = [utils_recop:get(MProtocol, Model, ModelKey) || ModelKey <- ModelKeyList],
  ValueConverted = apply(FConvert, ValueList),
  {RepoKey, ValueConverted};
get_one_key_value(MProtocol, Model, {RepoKey, ModelKey}) ->
  Value = utils_recop:get(MProtocol, Model, ModelKey),
  {RepoKey, Value}.
%%-------------------------------------------------------------------
do_model_2_model(MFrom, MTo, Config, Model) when is_atom(MFrom), is_atom(MTo), is_list(Config), is_tuple(Model) ->
  F = fun
        ({KeyTo, KeyFrom}, AccIn) when is_atom(KeyTo), is_atom(KeyFrom) ->
          Value = utils_recop:get(MFrom, Model, KeyFrom),
          [{KeyTo, Value} | AccIn];

        ({KeyTo, {static, Value}}, AccIn) when is_atom(KeyTo) ->
          [{KeyTo, Value} | AccIn];

        ({KeyTo, {element, N, KeyFrom}}, AccIn) when is_atom(KeyTo), is_atom(KeyFrom) ->
          Value = utils_recop:get(MFrom, Model, KeyFrom),
          ValueElement = element(N, Value),
          [{KeyTo, ValueElement} | AccIn];

        ({KeyTo, Fun, []}, AccIn) when is_atom(KeyTo), is_function(Fun, 0) ->
          Value = apply(Fun, []),
          [{KeyTo, Value} | AccIn];

        ({KeyTo, {tuple, KeyFromList}}, AccIn) when is_atom(KeyTo), is_list(KeyFromList) ->
          VL = [utils_recop:get(MFrom, Model, Key) || Key <- KeyFromList],
          [{KeyTo, list_to_tuple(VL)} | AccIn];

        ({KeyTo, Fun, [KeyFrom]}, AccIn) when is_atom(KeyTo), is_atom(KeyFrom), is_function(Fun, 1) ->
          ValueFrom = utils_recop:get(MFrom, Model, KeyFrom),
          Value = apply(Fun, [ValueFrom]),
          [{KeyTo, Value} | AccIn]

      end,

  VL = lists:foldl(F, [], Config),
  VL.
%%-------------------------------------------------------------------
-spec model_2_model_list(MTo, ModelList) -> ModelNew when
  MTo :: protocol_modules(),
  ModelList :: [protocol_models()],
  ModelNew :: protocol_models().
model_2_model_list(MTo, ModelList) when is_atom(MTo), is_list(ModelList) ->
  model_2_model_list(MTo, ModelList, default).

-spec model_2_model_list(MTo, ModelList, OpName) -> ModelNew when
  MTo :: protocol_modules(),
  ModelList :: [protocol_models()],
  OpName :: atom(),
  ModelNew :: protocol_models().
model_2_model_list(MTo, ModelList, OpName) when is_atom(MTo), is_list(ModelList), is_atom(OpName) ->
  ConfigList = apply(MTo, model_2_model_list_config, []),
  Config = proplists:get_value(OpName, ConfigList),
  true = length(ModelList) =:= length(Config),

  F = fun(I) ->
    Model = lists:nth(I, ModelList),
    {MCopyFrom, MCopyTo, CopyOpList} = lists:nth(I, Config),
    do_model_2_model(MCopyFrom, MCopyTo, CopyOpList, Model)
      end,


  VL = [F(I) || I <- lists:seq(1, length(ModelList))],

  VLFlatten = lists:flatten(VL),

  utils_recop:new(MTo, VLFlatten).

%%-------------------------------------------------------------------
is_first_success(ModelPayResp) ->
  UpIndexKey = utils_recop:get(protocol_up_resp_pay, ModelPayResp, up_index_key),
  {ok, [RepoPay]} = behaviour_repo:fetch_index(repo_up_txn_log_pt, {up_index_key, UpIndexKey}),
  OrigTxnStatus = utils_recop:get(repo_up_txn_log_pt, RepoPay, txn_status),
  FirstSuccess = (OrigTxnStatus =:= waiting),
  FirstSuccess.

is_first_success(UpIndexKey, success) ->
  {ok, [RepoPay]} = behaviour_repo:fetch_index(repo_up_txn_log_pt, {up_index_key, UpIndexKey}),
  OrigTxnStatus = utils_recop:get(repo_up_txn_log_pt, RepoPay, txn_status),
  FirstSuccess = (OrigTxnStatus =:= waiting),
  FirstSuccess;
is_first_success(_, _) ->
  %% resp code is not 00
  %% no need update quota
  false.


update_quota(_Repo, false) ->
%% already success, no need to update
  ok;
update_quota(Repo, true) ->
  {MchtIdBin, TxnDate, _} = utils_recop:get(repo_up_txn_log_pt, Repo, mcht_index_key),
  TxnAmt = utils_recop:get(repo_up_txn_log_pt, Repo, up_txnAmt),
  gws_quota:update(binary_to_integer(MchtIdBin), pay, TxnDate, TxnAmt).

%%-------------------------------------------------------------------
-spec save(M, Model) -> ok when
  M :: protocol_modules(),
  Model :: protocol_models().

save(M = protocol_up_resp_pay, Model) when element(1, Model) =:= M ->

%%  FirstSuccess = is_first_success(ModelRespPay),
  UpIndexKey = utils_recop:get(M, Model, up_index_key),
  TxnStatus = xfutils:up_resp_code_2_txn_status(utils_recop:get(M, Model, respCode)),
  FirstSuccess = is_first_success(UpIndexKey, TxnStatus),

  RepoNew = model_2_repo(up, M, Model),

  update_quota(RepoNew, FirstSuccess),

  RepoNew;

save(M = protocol_up_resp_query, Model) when element(1, Model) =:= M ->
  UpIndexKey = utils_recop:get(protocol_up_resp_query, Model, up_index_key),
  TxnStatus = xfutils:up_resp_code_2_txn_status(utils_recop:get(protocol_up_resp_query, Model, origRespCode)),
  FirstSuccess = is_first_success(UpIndexKey, TxnStatus),
  RepoNew = model_2_repo(up, M, Model),

  update_quota(RepoNew, FirstSuccess),

  RepoNew;

save(M = protocol_up_req_pay, Model) when element(1, Model) =:= M ->
  RepoNew = model_2_repo_pk(up, M, Model),
  RepoNew;

save(M, Model) when
  (
      (M =:= protocol_mcht_req_pay)
        or (M =:= protocol_mcht_req_refund)
        or (M =:= protocol_mcht_resp_refund)
        or (M =:= protocol_mcht_resp_pay)
        or (M =:= protocol_mcht_notify_refund)
  ),
  element(1, Model) =:= M ->
  RepoNew = model_2_repo_pk(mcht, M, Model),
  RepoNew.

%%save(M = protocol_mcht_req_refund, Model) when element(1, Model) =:= M ->
%%  RepoNew = model_2_repo_pk(mcht, M, Model),
%%  RepoNew;
%%
%%save(M = protocol_mcht_resp_refund, Model) when element(1, Model) =:= M ->
%%  RepoNew = model_2_repo_pk(mcht, M, Model),
%%  RepoNew;
%%
%%save(M = protocol_mcht_resp_pay, Model) when element(1, Model) =:= M ->
%%  RepoNew = model_2_repo_pk(mcht, M, Model),
%%  RepoNew.


%%-------------------------------------------------------------------
-spec fetch(M, PkValue) -> {ok, [Model]} | {ok, []} when
  M :: protocol_modules(),
  PkValue :: any(),
  Model :: protocol_models().

fetch(M, PkValue) when is_atom(M) ->
  case behaviour_repo:fetch(repo_up_txn_log_pt, PkValue) of
    {ok, []} -> {ok, []};
    {ok, [Repo]} ->
      {ok, Model} = repo_2_model(repo_up_txn_log_pt, M, Repo),
      {ok, [Model]}
  end.
%%-------------------------------------------------------------------
-spec fetch_index(M, {IndexName, IndexValue}) -> {ok, [Model]} | {ok, []} when
  M :: protocol_modules(),
  IndexName :: atom(),
  IndexValue :: any(),
  Model :: protocol_models().

fetch_index(M, {IndexName, IndexValue}) when is_atom(M) ->
  case behaviour_repo:fetch_index(repo_up_txn_log_pt, {IndexName, IndexValue}) of
    {ok, []} -> {ok, []};
    {ok, [Repo]} ->
      {ok, Model} = repo_2_model(repo_up_txn_log_pt, M, Repo),
      {ok, [Model]}
  end.
%%==================================================================
-spec up_sign_string(M, Model, FieldList) -> Sig when
  M :: protocol_modules(),
  Model :: protocol_models(),
  FieldList :: list(),
  Sig :: binary().

up_sign_string(M, Model, FieldList) when is_atom(M), is_list(FieldList) ->
  A = [
    prepare_one_sign_field(X, utils_recop:get(M, Model, X))
    || X <- FieldList
  ],
  list_to_binary(A).

%% last field , not add tail & character

prepare_one_sign_field(version = X, Value) ->
  [atom_to_list(X), <<"=">>, Value];
prepare_one_sign_field(settleDate, <<"0000">>) ->
  [];
prepare_one_sign_field(_X, EmptyValue)
  when (EmptyValue =:= <<>>)
  or (EmptyValue =:= undefined)
  ->
  % if field value is empty, not put in sign_string
  [];
prepare_one_sign_field(X, Value) when is_integer(Value) ->
  [atom_to_list(X), <<"=">>, integer_to_binary(Value), <<"&">>];
prepare_one_sign_field(X, Value) when is_binary(Value);is_list(Value) ->
  [atom_to_list(X), <<"=">>, Value, <<"&">>].
%%==================================================================
-spec up_sign(binary(), tuple()) -> binary().
up_sign(Bin, Key) ->
%%  %S = public_key:sign({digest,Bin},'sha',Key),
  S = public_key:sign(Bin, 'sha', Key),
  B = base64:encode(S),
  B.

up_sign_test() ->
  xfutils:app_env_init_for_test(),
%%  Key = unionpay_config:privateKey(999),
  Key = up_config:get_mer_prop(test,privateKey),
  DigestBin = <<"c527432e8f632d555c651eaf8e5e0b027405fa46">>,
  SignResult = up_sign(DigestBin, Key),
  ?assertEqual(SignResult, <<"hLrvwNR5lHtQUKU/tXvwY857BKbKOQCGrKbLcg/68ZIG04yGnMFcmKUOO/cAt7SREoyjbOmSR6FUficTeofXZr7arrujbtyBB3wolmQ0BKTAVA7jjYaaYmMQxRkSlMElX7kYSsZyqCAzIzDkqKA36nN9w5g2wur3fbkha7GPyIw=">>),
  ok.

%%==================================================================
-spec digest_string_upper(binary()) -> binary().
digest_string_upper(Bin) ->
  DigestBin = crypto:hash(sha, Bin),
  DigestHex = xfutils:bin_to_hex(DigestBin),
  % convert to lowercase
  DigestHex.


%%==================================================================
-spec digest_string(binary()) -> binary().
digest_string(Bin) ->
  U = digest_string_upper(Bin),
  list_to_binary(string:to_lower(binary_to_list(U))).

digest_string_test() ->
  A = <<"accNo=6225682141000002950&accessType=0&backUrl=https://101.231.204.80:5000/gateway/api/backTransReq.do&bizType=000201&certId=124876885185794726986301355951670452718&channelType=07&currencyCode=156&encoding=UTF-8&merId=898340183980105&orderId=2014110600007615&signMethod=01&txnAmt=000000010000&txnSubType=01&txnTime=20150109135921&txnType=01&version=5.0.0">>,

  ?assertEqual(digest_string(A), <<"c527432e8f632d555c651eaf8e5e0b027405fa46">>).

%%==================================================================
-spec up_sign_msg(M, Model, FieldList) -> Sig when
  M :: protocol_modules(),
  Model :: protocol_models(),
  FieldList :: list(),
  Sig :: binary().

up_sign_msg(M, Model, SignFields) when is_atom(M), is_list(SignFields) ->
  MerId = utils_recop:get(M, Model, merId),
%%  ShortMerId = unionpay_config:short_mer_id(MerId),
  SignString = up_sign_string(M, Model, SignFields),
  lager:debug("SignString =~ts", [SignString]),
  DigestBin = digest_string(SignString),
%%  Key = unionpay_config:privateKey(ShortMerId),
  Key = up_config:get_mer_prop(MerId,privateKey),
  SignBin = up_sign(DigestBin, Key),
  SignBin.

%%=============================================================================
-spec signature_decode(binary()) -> binary().
signature_decode(Signature) ->
  base64:decode(Signature).
%%==================================================================
-spec up_verify_msg(M, Model, FieldList) -> boolean() when
  M :: protocol_modules(),
  Model :: protocol_models(),
  FieldList :: list().

up_verify_msg(M, Model, SignFields) when is_atom(M), is_list(SignFields) ->
  SignString = up_sign_string(M, Model, SignFields),
  lager:debug("SignString = ~ts", [SignString]),
  DigestResp = digest_string(SignString),

  %#?UP_RESP{signature = Signature} = UpResp,
  Signature = utils_recop:get(M, Model, signature),
  SignatureDecode = signature_decode(Signature),
%%  PK = unionpay_config:public_key(),
  PK = up_config:public_key(),
  lager:debug("Resp's sign string digesst = ~ts~n,signature = ~ts", [DigestResp, Signature]),

  VerifyRet = public_key:verify(DigestResp, sha, SignatureDecode, PK),
  case VerifyRet of
    true -> ok;
    false ->
      % verify fail
      UpIndexKey = utils_recop:get(M, Model, up_index_key),
      lager:error("Up Txn ~p sig verify failed. SignString = ~ts,Sig = ~ts",
        [UpIndexKey, SignString, Signature]),
      fail
  end.
%%==================================================================
-spec mcht_verify_msg(M, Model, FieldList) -> Sig when
  M :: protocol_modules(),
  Model :: protocol_models(),
  FieldList :: list(),
  Sig :: binary().

mcht_verify_msg(M, Model, SignFields) when is_atom(M), is_list(SignFields) ->
  MchtId = utils_recop:get(M, Model, mcht_id),
  SignString = mcht_sign_string(M, Model, SignFields),
  Signature = utils_recop:get(M, Model, signature),
  lager:debug("In mcht req, SignString = ~ts,Sig = ~p", [SignString, Signature]),


  Direction = sign_mcht_direction(M),

  case gw_enc:verify_hex(binary_to_integer(MchtId),
    Direction, SignString, Signature) of
    true -> ok;
    false ->
      % verify fail
      TxnDate = utils_recop:get(M, Model, mcht_txn_date),
      TxnSeq = utils_recop:get(M, Model, mcht_txn_seq),
      lager:error("sig verify failed. SignString = ~ts,Sig = ~ts,in txnDate = ~ts,txnSeq = ~ts",
        [SignString, Signature, TxnDate, TxnSeq]),
      fail
    %throw({sig_verify_failed, {MchtId, TxnDate, TxnSeq, SignString, Signature}})
  end.

%%==================================================================
-spec mcht_sign_msg(M, Model, FieldList) -> {SignStr, Sig} when
  M :: atom(),
  Model :: model_mcht_req_pay:req_mcht() | model_mcht_resp_pay:resp_mcht(),
  FieldList :: [model_mcht_req_pay:req_mcht_fields() |
  model_mcht_resp_pay:resp_mcht_fields()],
  SignStr :: binary(),
  Sig :: model_mcht_req_pay:mcht_signature().


mcht_sign_msg(M, Model, SignFields) when is_atom(M), is_list(SignFields) ->
  MchtId = utils_recop:get(M, Model, mcht_id),
  SignString = mcht_sign_string(M, Model, SignFields),

  Direction = sign_mcht_direction(M),
  Sig = gw_enc:sign_hex(binary_to_integer(MchtId), Direction, SignString),
  lager:debug("In mcht resp, SignString = ~ts,Sig = ~ts", [SignString, Sig]),
  {SignString, Sig}.

%%==================================================================
sign_mcht_direction(protocol_mcht_req_refund) ->
  req;
sign_mcht_direction(protocol_mcht_resp_refund) ->
  resp;
sign_mcht_direction(protocol_mcht_info_refund) ->
  resp;
sign_mcht_direction(protocol_mcht_notify_refund) ->
  resp;
sign_mcht_direction(protocol_mcht_req_query) ->
  req;
sign_mcht_direction(protocol_mcht_resp_query) ->
  resp;
sign_mcht_direction(protocol_mcht_req_pay) ->
  req;
sign_mcht_direction(protocol_mcht_resp_pay) ->
  resp.


%%==================================================================
-spec mcht_sign_string(M, Model, FieldList) -> SignString when
  M :: atom(),
  Model :: model_mcht_req_pay:req_mcht() | model_mcht_resp_pay:resp_mcht(),
  FieldList :: [model_mcht_req_pay:req_mcht_fields() |
  model_mcht_resp_pay:resp_mcht_fields()],
  SignString :: binary().

mcht_sign_string(M, Model, SignFields) when is_atom(M), is_list(SignFields) ->
  A = [
    value_for_sign(utils_recop:get(M, Model, X))
    || X <- SignFields
  ],
  list_to_binary(A).

value_for_sign(undefined) ->
  <<>>;
value_for_sign(Value) when is_integer(Value) ->
  integer_to_binary(Value);
value_for_sign(Value) ->
  Value.

sign_string_test() ->
  PropList = [
    {<<"tranAmt">>, <<"10000">>}
    , {<<"orderDesc">>, <<"{pI=131026}"/utf8>>}
    , {<<"orderId">>, <<"1460028751003">>}
    , {<<"merchId">>, <<"000121">>}
    , {<<"tranId">>, <<"193231">>}
    , {<<"prodId">>, <<"131026">>}
    , {<<"tranDate">>, <<"20160407">>}
    , {<<"tranTime">>, <<"193231">>}
    , {<<"accountId">>, <<"03429500040006212">>}
    , {<<"accountName">>, <<"上海聚孚金融信息服务有限公司"/utf8>>}
    , {<<"accountBank">>, <<"6225220113392775">>}
    , {<<"signature">>, <<"test">>}
    , {<<"gateWayId">>, <<"1002">>}
    , {<<"trustFrontUrl">>, <<"http://test.trust-one.com/payment_success/148641458285747438">>}
    , {<<"trustBackUrl">>, <<"http://test.trust-one.com/dapi/gateWay/pay/notice">>}
    , {<<"bankId">>, <<"ICBCD">>}
  ],
  ModelReqPay = out_2_model(protocol_mcht_req_pay, PropList),
  SignFields = sign_fields(protocol_mcht_req_pay),
  SignString = mcht_sign_string(protocol_mcht_req_pay, ModelReqPay, SignFields),
  %ExpectString = <<"00012120160407193231193231146002875100303429500040006212%E4%B8%8A%E6%B5%B7%E8%81%9A%E5%AD%9A%E9%87%91%E8%9E%8D%E4%BF%A1%E6%81%AF%E6%9C%8D%E5%8A%A1%E6%9C%89%E9%99%90%E5%85%AC%E5%8F%B86225220113392775100{pI=131026}1002http://test.trust-one.com/dapi/gateWay/pay/noticehttp://test.trust-one.com/dapi/gateWay/pay/notice131026ICBCD"/utf8>>,
  ExpectString = <<"0001212016040719323119323110000ICBCD{pI=131026}1002http://test.trust-one.com/dapi/gateWay/pay/noticehttp://test.trust-one.com/payment_success/14864145828574743813102603429500040006212上海聚孚金融信息服务有限公司6225220113392775"/utf8>>,
  lager:debug("~nSignString = ~ts", [SignString]),
  lager:debug("~nExpectSignString = ~ts", [ExpectString]),
  ?assertEqual(SignString, ExpectString).

%% test
string_utf8_test() ->
  PL = [{<<"orderDesc">>, <<"{pI=test,aI=03429500040006212,aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行}"/utf8>>}],
  {_, P1} = proplists:lookup(<<"orderDesc">>, PL),
  A = <<"{pI=test,aI=03429500040006212,aN=上海聚孚金融信息服务有限公司,aB=农业银行上海张江集电港支行}"/utf8>>,
  ?assertEqual(A, P1),
  ok.