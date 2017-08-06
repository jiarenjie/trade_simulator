%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2017 9:20 PM
%%%-------------------------------------------------------------------
-module(behaviour_txn_process).
-author("simon").

%% API
-export([process/2]).
%% callbacks
-callback validate_req(PostVals :: any()) -> map().
-callback create_req_model(PostVals :: any()) -> map().
-callback validate_req_model(Model :: tuple()) -> map().
-callback save_req_model(Model :: tuple()) -> tuple().
-callback create_resp_model(Model :: tuple()) -> map().
-callback save_resp_model(Model :: tuple()) -> map().
-callback render_success_resp_model(Model :: tuple()) -> map().
-callback render_fail_resp_model(RespCd :: binary(), RespMsg :: binary()) -> {ok, binary()}.
-callback render_fail_resp_model(RespCd :: binary(), RespMsg :: binary(), Model :: tuple()) -> {ok, binary()}.


%%-------------------------------------------------------------------
-define(LARGER_STACKTRACE_1(X),
  lager:error("Error =~p,stacktrace=~s", [X, lager:pr_stacktrace(erlang:get_stacktrace())])).
%%-------------------------------------------------------------------
fun_validate_req(M, PostVals) when is_atom(M) ->
  case apply(M, validate_req, [PostVals]) of
    {ok, _, _} ->
      %% validate ok, return postvals for next step
      PostVals;
    {fail, RespCd, RespMsg} ->
      %% infor outer catch
      throw({validate_stop, RespCd, RespMsg})
  end.


fun_create_req_model(M, PostVals) when is_atom(M)
  , (is_list(PostVals)
    or is_tuple(PostVals))
  ->
  try
    apply(M, create_req_model, [PostVals])
  catch
    _ : X ->
%%      lager:error("Error = ~p,stack = ~p", [X, erlang:get_stacktrace()]),
      ?LARGER_STACKTRACE_1(X),
      lager:error("validate req model error. PostVals = ~p", [PostVals]),
      throw({create_req_model_stop, <<"99">>, <<"请求协议转换错误"/utf8>>})
  end.

fun_validate_req_model(M, Model) when is_atom(M), is_tuple(Model) ->
  try
    apply(M, validate_req_model, [Model])
  catch
    throw :{validate_fail, RespCd, RespMsg} ->
      lager:error("validate req model fail! RespCd = ~p,RespMsg = ~ts,Model =~p",
        [RespCd, RespMsg, Model]),
      throw({validate_req_model_stop, RespCd, RespMsg, Model});
    _:X ->
      lager:error("Error = ~p,stack = ~s", [X, lager:pr_stacktrace(erlang:get_stacktrace())]),
      lager:error("validate req model error. Model = ~p", [Model]),
      throw({validate_req_model_stop, <<"99">>, <<"请求交易验证失败"/utf8>>, Model})
  end.

fun_save_req_model(M, Model) when is_atom(M), is_tuple(Model) ->
  try
    apply(M, save_req_model, [Model])

  catch
    _ :X ->
%%      lager:error("Error = ~p,stack = ~p", [X, erlang:get_stacktrace()]),
      ?LARGER_STACKTRACE_1(X),
      lager:error("save req model error. Model = ~p", [Model]),
      throw({save_req_model_stop, <<"99">>, <<"保存交易请求错误"/utf8>>})
  end.

fun_create_resp_model(M, Model) when is_atom(M), is_tuple(Model) ->
  try
    apply(M, create_resp_model, [Model])
  catch
    _:{RespCd, RespMsg} when is_binary(RespCd), is_binary(RespMsg) ->
      throw({save_req_model_stop, RespCd, RespMsg});

    _ :X ->
%%      lager:error("Error = ~p,stack = ~p", [X, erlang:get_stacktrace()]),
      ?LARGER_STACKTRACE_1(X),
      lager:error("create resp model error. Model = ~p", [Model]),
      throw({save_req_model_stop, <<"99">>, <<"生成交易应答错误"/utf8>>})
  end.


fun_save_resp_model(M, Model) when is_atom(M), is_tuple(Model) ->
  try
    apply(M, save_resp_model, [Model])
  catch
    _ :X ->
%%      lager:error("Error = ~p,stack = ~p", [X, erlang:get_stacktrace()]),
      ?LARGER_STACKTRACE_1(X),
      lager:error("create resp model error. Model = ~p", [Model]),
      throw({save_resp_model_stop, <<"99">>, <<"保存交易应答错误"/utf8>>})
  end.

fun_render_success_resp_model(M, Model) when is_atom(M), is_tuple(Model) ->
  try
    apply(M, render_success_resp_model, [Model])
  catch
    _ :X ->
%%      lager:error("Error = ~p,stack = ~p", [X, erlang:get_stacktrace()]),
      ?LARGER_STACKTRACE_1(X),
      lager:error("create resp model error. Model = ~p", [Model]),
      throw({render_success_resp_model, <<"99">>, <<"生成交易成功应答报文错误"/utf8>>})
  end.

fun_render_fail_resp_model(M, RespCd, RespMsg) when is_atom(M), is_binary(RespCd), is_binary(RespMsg) ->
  try
    apply(M, render_fail_resp_model, [RespCd, RespMsg])
  catch
    _ :X ->
%%      lager:error("Error = ~p,stack = ~p", [X, erlang:get_stacktrace()]),
      ?LARGER_STACKTRACE_1(X),
      throw({render_fail_resp_model, <<"99">>, <<"生成交易失败应答报文错误"/utf8>>})
  end.
fun_render_fail_resp_model(M, RespCd, RespMsg, Model) when is_atom(M), is_binary(RespCd), is_binary(RespMsg), is_tuple(Model) ->
  try
    apply(M, render_fail_resp_model, [RespCd, RespMsg, Model])
  catch
    _ :X ->
%%      lager:error("Error = ~p,stack = ~p", [X, erlang:get_stacktrace()]),
      ?LARGER_STACKTRACE_1(X),
      throw({render_fail_resp_model, <<"99">>, <<"生成交易失败应答报文错误"/utf8>>})
  end.
%%-------------------------------------------------------------------
-spec process(M, PostVals) -> Result when
  M :: atom(),
  PostVals :: proplists:proplist() | tuple(),
  Result :: any().

process(M, PostVals) when is_atom(M) ->
  Pipeline = [
    {validate_req, fun fun_validate_req/2}
    , {create_req_model, fun fun_create_req_model/2}
    , {validate_req_model, fun fun_validate_req_model/2}
    , {save_req_model, fun fun_save_req_model/2}
    , {create_resp_model, fun fun_create_resp_model/2}
    , {save_resp_model, fun fun_save_resp_model/2}
  ],

  F = fun
        ({OpName, Handler}, Acc) when is_atom(OpName), is_function(Handler) ->
%%          lager:debug(" === in ~p,Acc=~p", [OpName, Acc]),
          AccNew = Handler(M, Acc),
%%          lager:debug("AccNew=~p", [AccNew]),
          AccNew
      end,

  Resp = try
           {RespModel, _} = lists:foldl(F, PostVals, Pipeline),
           {ok, BodySuccess} = fun_render_success_resp_model(M, RespModel),
           lager:debug("BodySuccess = ~p", [BodySuccess]),
           {200, BodySuccess}

         catch
           throw:{Atom, RespCd, RespMsg}
             ->
             %% fail @ validate/create_req_model
%%             xfutils:post_vals_to_iolist([{resp_code, RespCd}, {resp_msg, RespMsg}]);
             lager:error("txn process error = ~p,RespCode = ~p,RespMsg = ~ts", [Atom, RespCd, RespMsg]),
             {ok, BodyFail} = fun_render_fail_resp_model(M, RespCd, RespMsg),
             {200, BodyFail};

           throw:{Atom, RespCd, RespMsg, Model}
             ->
%% fail @ validate_req_model
%%             xfutils:post_vals_to_iolist([{resp_code, RespCd}, {resp_msg, RespMsg}])
             lager:error("txn process error = ~p,RespCode = ~p,RespMsg = ~ts,Model = ~p", [Atom, RespCd, RespMsg, Model]),
             {ok, BodyFail} = fun_render_fail_resp_model(M, RespCd, RespMsg, Model),
             {200, BodyFail}

         end,
  Resp.


