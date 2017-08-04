%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 七月 2017 14:43
%%%-------------------------------------------------------------------
-module(pay_simulator).
-author("pingjianwei").
-include_lib("xmerl/include/xmerl.hrl").
%% 为什么不加../会变红呢？
-include("../include/store.hrl").

%% API
-export([send_mcht_req/0,pay_simulator_start/2]).


pay_simulator_start(TimesPerSecond,TotalTimes) ->
  F = fun(_X,[A,Acc])->
    lager:info("world is beatifull!"),
    pay_sup:start_child(),
    case A-1 =:= 0 of
      false ->[A-1,Acc-1];
      true ->
        timer:sleep(1000),
        [A,Acc-1]
    end
    end,
  lists:foldl( F,[TimesPerSecond,TotalTimes],lists:seq(1,TotalTimes)).

sign_feilds() ->
  [merchId,tranDate,tranId,tranTime,tranAmt,orderDesc,trustBackUrl,trustFrontUrl].

init_mcht_req()->
  [
    {merchId,<<"00002">>}
    ,{tranDate, xfutils:today()}
    ,{tranId,erlang:list_to_binary( xfutils:now(ts))}
    ,{tranTime,erlang:list_to_binary(lists:sublist(xfutils:now(txn),9,14))}
    ,{tranAmt,<<"100">>}
    ,{orderDesc,<<"{pI=test,aI=03429500040006212}">>}
    ,{trustBackUrl,<<"http://test.trust-one.com/pg/simu_mcht_back_succ_info">>}
    ,{trustFrontUrl,<<"http://test.trust-one.com/pg/simu_mcht_front_succ">>}
  ].


send_mcht_req() ->
  ReqData =init_mcht_req(),
  SignStr=list_to_binary([proplists:get_value(X,ReqData)|| X<-sign_feilds()]),
  {PrivateKey,_} = get_private_key("src/keys/private_key.pem", ""),
  Signature = sign_hex(SignStr, PrivateKey),
  PostVals = lists:flatten([ReqData,{signature,Signature}]),
  PostString = xfutils:post_vals_to_string(PostVals),
  %lager:info("PostString=~p",[PostString]),
  Url = "http://localhost:8888/pg/pay/",
 % RequestResult = httpc:request(post,{Url, [], "application/x-www-form-urlencoded", PostString}, [], []),
  %lager:info("Notify result = ~p~n ,post_vals=~p", [RequestResult, PostVals])

  case httpc:request(post,{Url, [], "application/x-www-form-urlencoded", PostString}, [], []) of
    {ok,{{_,RespCode,_},_,Body}} ->
      XmlElt = parse_up_html(Body),
%%      record_req_log(ReqData,XmlElt,integer_to_binary(RespCode));
     save(ReqData,XmlElt,RespCode);
    _-> save(ReqData,[{<<>>,<<>>}],<<"fail_connect">>)
  end,
  up_sup:start_child(proplists:get_value(tranId,ReqData))

  %{ok,{_,_,Body}} = httpc:request(post,{Url, [], "application/x-www-form-urlencoded", PostString}, [], []),
  %XmlElt = parse_up_html(Body)
  .

parse_up_html(UpHtml) ->
  UpHtmlBin = binary:replace(list_to_binary(UpHtml),
     [<<"\n<meta http-equiv=\"content-type\" content=\"text/html;charset=Utf-8\">">>
      ,<<"\n<meta charset=\"UTF-8\">">>],<<>>),
  {XmlElt, _} = xmerl_scan:string( binary_to_list(UpHtmlBin)),
  Items = xmerl_xpath:string("/html/body/form/input", XmlElt),
  G = fun(Item) ->
    [#xmlAttribute{value = Name}] = xmerl_xpath:string("/input/@name", Item),
    [#xmlAttribute{value = Value}] = xmerl_xpath:string("/input/@value", Item),
    {Name,Value}
    end,
  [G(X)|| X<-Items].


get_private_key(KeyFileName, Pwd) ->
  try
    {ok, PemBin} = file:read_file(KeyFileName),
    [RSAEntry | _Rest] = public_key:pem_decode(PemBin),
    RsaKeyInfo = public_key:pem_entry_decode(RSAEntry, Pwd),
    {RsaKeyInfo, PemBin}

  catch
    error :X ->
      lager:error("read private key file ~p error! Msg = ~p", [KeyFileName, X]),
      {<<>>, <<>>}
  end.

sign_hex(DigestBin, PrivateKey) ->
  SignedBin = public_key:sign(DigestBin, sha, PrivateKey),
  Hex = xfutils:bin_to_hex(SignedBin),
  Hex.


save(ReqData,RespData,RespCode) ->
  TxnLog = #txn_log{
    mcht_txn_seq = proplists:get_value(tranId,ReqData)
    , mcht_txn_date = proplists:get_value(tranDate,ReqData)
    , mcht_txn_time = proplists:get_value(tranTime,ReqData)
    , up_orderId = proplists:get_value("orderId",RespData,<<"null">>)
    , up_txnAmt = proplists:get_value("txnAmt",RespData,<<"null">>)
    , txn_statue = waiting
    , up_respCode = RespCode
  },
  behaviour_repo:save(TxnLog).


record_req_log(ReqData,RespData,RespCode) ->
  Result = file:write_file("reqlog.txt"
    ,[
      proplists:get_value(tranTime,ReqData)
      ,<<"|">>
      ,proplists:get_value(tranId,ReqData)
      ,<<"|">>
      ,proplists:get_value("orderId",RespData,<<"null">>)
      ,<<"|">>
      ,proplists:get_value("txnAmt",RespData,<<"null">>)
      ,<<"|">>
      ,RespCode
      ,<<"\r\n">>
    ]
    ,[append]),

  lager:info("write result= ~p~n write content = ~p",[Result,[
    proplists:get_value(tranTime,ReqData)
    ,proplists:get_value(tranId,ReqData)
    ,proplists:get_value("orderId",RespData)
    ,proplists:get_value("txnAmt",RespData)
    ,RespCode
  ]]).


