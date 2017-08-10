%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2016 17:21
%%%-------------------------------------------------------------------
-module(wh_txn_pay_back_pt).
-author("simonxu").

%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, Req, []) ->
  {Reply, Req2} = xfutils:only_allow(post, Req),
  {Reply, Req2, no_state}.

terminate(_Reason, _Req, _State) ->
  ok.

handle(Req, State) ->

  %% get query string
  {ok, PostVals, Req2} = xfutils:post_get_qs(Req),
  lager:debug("in /pay_succ_front, PostVals = ~p", [PostVals]),

  {StatusCode, _ReplyBody} = process_back_notice(PostVals),

  {ok, Req3} = cowboy_req:reply(StatusCode, [{<<"content-type">>, <<"text/html">>}], [], Req2),

  {ok, Req3, State}.

process_back_notice(PostVals) ->
  behaviour_repo:update_pk(repo_txn_log_pt,proplists:get_value(<<"tranId">>,PostVals),[{txn_statue,success}]),
  {200,PostVals}.


