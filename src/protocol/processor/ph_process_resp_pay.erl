%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2017 9:18 PM
%%%-------------------------------------------------------------------
-module(ph_process_resp_pay).
-include_lib("eunit/include/eunit.hrl").
-behavior(behaviour_txn_process).
-author("simon").

%% API
%% callbacks
-export([
  validate_req/1
  , create_req_model/1
  , validate_req_model/1
  , save_req_model/1
  , create_resp_model/1
  , save_resp_model/1
  , render_success_resp_model/1
  , render_fail_resp_model/2
  , render_fail_resp_model/3
]).

-compile(export_all).

%%-define(BHProtocol, behaviour_protocol_model).
%%-define(BHRepo, behaviour_repo).
%%-define(M_MCHANTS, repo_mchants_pt).
%%-define(MReq, protocol_mcht_req_pay).

%%-------------------------------------------------------------------
validate_req(PostVals) when is_list(PostVals) ->
  ph_validate:validate_req_fields(mcht, pay, PostVals).

create_req_model(PostVals) when is_list(PostVals) ->
  behaviour_protocol_model:out_2_model(protocol_mcht_req_pay, PostVals).

validate_req_model(Model) when is_tuple(Model) ->
  % mcht_id check
  ok = ph_validate:mcht_id_check(protocol_mcht_req_pay, Model),
  % dup id check
  ok = ph_validate:dup_id_check(protocol_mcht_req_pay, Model),

  % sig verify
  ok = ph_validate:verify_sig(mcht, protocol_mcht_req_pay, Model),

  %% quota check
  ok = quota_check(protocol_mcht_req_pay, Model),

  %% bnakid check
  ok = ph_validate:payment_method_check(protocol_mcht_req_pay, Model),

  %% txn amt limit check
  ok = ph_validate:txn_amt_limit_check(protocol_mcht_req_pay, Model),

  Model.

save_req_model(Model) when is_tuple(Model) ->
%%  RepoMcht = behaviour_protocol_model:model_2_repo_pk(mcht, protocol_mcht_req_pay, Model),
  RepoMcht = behaviour_protocol_model:save(protocol_mcht_req_pay, Model),
%%  ok = behaviour_repo:save(RepoMcht),
  {Model, RepoMcht}.

%% create up req pay model from mcht req model
create_resp_model({Model, RepoMchtTxnLog}) when is_tuple(Model), is_tuple(RepoMchtTxnLog) ->
  ModelUpReqPay = behaviour_protocol_model:model_2_model_list(protocol_up_req_pay, [Model]),
%%  Sign = behaviour_protocol_model:sign(up, protocol_up_req_pay, ModelUpReqPay),
%%  ModelUpReqPayWithSig = utils_recop:set(protocol_up_req_pay, ModelUpReqPay, signature, Sign),
%%  ModelUpReqPayWithSig.
  behaviour_protocol_model:sign(ModelUpReqPay).

%% save up req pay model
save_resp_model(ModelUpReqPay) when is_tuple(ModelUpReqPay) ->
%%  RepoUp = behaviour_protocol_model:model_2_repo_pk(up, protocol_up_req_pay, ModelUpReqPay),
  RepoUp = behaviour_protocol_model:save(protocol_up_req_pay, ModelUpReqPay),
  {ModelUpReqPay, RepoUp}.


%%%% convert up req pay model to post qs
%%convert_resp_model({ModelUpRespPay, _RepoUp}) when is_tuple(ModelUpRespPay) ->
%%  ok.

%% render html @ success , return post form for unionpay pay req
render_success_resp_model(ModelUpRespPay) when is_tuple(ModelUpRespPay) ->
  UpPostVals = utils_recop:to_proplists(protocol_up_req_pay, ModelUpRespPay),
  Options = get_options(),
  lager:debug("Render options = ~p", [Options]),
  up_pay_dtl:render(UpPostVals ++ Options).

render_fail_resp_model(RespCd, RespMsg) when is_binary(RespCd), is_binary(RespMsg) ->
  ErrorVals = [{error_code, RespCd}, {error_msg, RespMsg}],
  Options = get_options(),
  error_req_dtl:render(ErrorVals).

render_fail_resp_model(RespCd, RespMsg, Model) when is_binary(RespCd), is_binary(RespMsg), is_tuple(Model) ->
  ErrorVals = [{error_code, RespCd}, {error_msg, RespMsg}],
  Options = get_options(),
  error_req_dtl:render(ErrorVals ++ Options).


%% =================
-spec quota_check(M, Model) -> ok when
  M :: atom(),
  Model :: protocol_mcht_req_pay:protocol_mcht_req_pay().
quota_check(M, Model) when is_atom(M) ->
  MchtId = utils_recop:get(M, Model, mcht_id),
  TxnAmt = utils_recop:get(M, Model, mcht_txn_amt),
  Pass = gws_quota:check(MchtId, pay, TxnAmt),
  case Pass of
    pass ->
      ok;
    out_of_quota ->
      % no quota
%%      {_MchtId, TxnDate, TxnSeq} = utils_recop:get(M, Model, mcht_index_key),
      throw({validate_fail, <<"99">>, <<"交易金额超限"/utf8>>})
  end.

%% -----------------
get_options() ->
  PauseShowSubmitButtonOptionValue = case application:get_env(pause_show_submit_button) of
                                       undefined ->
                                         <<"false">>;
                                       {ok, true} ->
                                         pauseAndShowSubmitButton, <<"true">>;
                                       {ok, _} ->
                                         pauseAndShowSubmitButton, <<"false">>
                                     end,

  [{pauseAndShowSubmitButton, PauseShowSubmitButtonOptionValue}].

