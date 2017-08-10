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
-include("../include/store.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([send_mcht_req/0,start/2,start/3]).
-compile(export_all).


%%simulator_start(TimesPerSecond,TotalTimes) ->
%%  F = fun(_X,[A,Acc])->
%%    pay_sup:start_child(),
%%    case A-1 =:= 0 of
%%      false ->[A-1,Acc-1];
%%      true ->
%%        timer:sleep(1000),
%%        [A,Acc-1]
%%    end
%%    end,
%%  lists:foldl( F,[TimesPerSecond,TotalTimes],lists:seq(1,TotalTimes)).

start(TimesPerSecond,TotalTimes)->
  start(TimesPerSecond,TotalTimes,TimesPerSecond).

start(_,0,_) ->
  ok;
start(0,TotalTimes,TimesPerSecond) ->
  timer:sleep(1000),
  start(TimesPerSecond,TotalTimes,TimesPerSecond);
start(TimesPerSecond,TotalTimes,TimesPerSecond2) ->
  pay_sup:start_child(),
  start(TimesPerSecond - 1 ,TotalTimes - 1 ,TimesPerSecond2).



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
    ,{trustBackUrl,<<"http://localhost:9999/pg/pay_succ_info">>}
    ,{trustFrontUrl,<<"http://localhost:9999/pg/pay_mcht_front">>}
  ].


send_mcht_req() ->
  ReqData =init_mcht_req(),
  SignStr=list_to_binary([proplists:get_value(X,ReqData)|| X<-sign_feilds()]),
  {PrivateKey,_} = simulator_enc:get_private_key("src/keys/private_key.pem", ""),
  Signature = simulator_enc:sign_hex(SignStr, PrivateKey),
  PostVals = lists:flatten([ReqData,{signature,Signature}]),
  PostString = xfutils:post_vals_to_string(PostVals),
  %lager:info("PostString=~p",[PostString]),
  Url = "http://localhost:8888/pg/pay/",
 % RequestResult = httpc:request(post,{Url, [], "application/x-www-form-urlencoded", PostString}, [], []),
  %lager:info("Notify result = ~p~n ,post_vals=~p", [RequestResult, PostVals])

  case httpc:request(post,{Url, [], "application/x-www-form-urlencoded", PostString}, [], []) of
    {ok,{{_,RespCode,_},_,Body}} ->
      UpValueLists = parse_up_html(Body),
%%      record_req_log(ReqData,XmlElt,integer_to_binary(RespCode));
     save(ReqData,UpValueLists,RespCode),
     record_req_log(ReqData,UpValueLists,RespCode);
    _-> save(ReqData,[{<<>>,<<>>}],<<"fail_connect">>)
  end,
  up_sup:start_child(proplists:get_value(tranId,ReqData))

  %{ok,{_,_,Body}} = httpc:request(post,{Url, [], "application/x-www-form-urlencoded", PostString}, [], []),
  %XmlElt = parse_up_html(Body)
  .

parse_up_html(UpHtml) ->
  {<<"html">>,[],[_, {<<"body">>,[],[ {<<"form">>,_,FormBody } ] } ] }= mochiweb_html:parse(UpHtml),
  lager:info("Body = ~p",[ FormBody]),
  G = fun(X,Acc) ->
    {_,PropList,_} =X,
    case proplists:get_value(<<"name">>,PropList) of
      undefined -> Acc;
      Key ->[{Key,proplists:get_value(<<"value">>,PropList)}|Acc]
    end
    end,
  lists:foldl(G,[],FormBody).


parse_up_html_old(UpHtml) ->
  UpHtml1 = lists:delete(lists:nth((length(UpHtml)-1),UpHtml),UpHtml),
  lager:info("UpHtml1 = ~p",[list_to_binary(UpHtml1)]),
  lager:info("~p",[lists:nth((length(UpHtml)-1),UpHtml)]),
  UpHtmlBin = binary:replace(list_to_binary(UpHtml1),
     [<<"\n<meta http-equiv=\"content-type\" content=\"text/html;charset=Utf-8\">">>
      ,<<"\n<meta charset=\"UTF-8\">">>],<<>>),

  {XmlElt, _} = xmerl_scan:string( binary_to_list(UpHtmlBin)),
  Items = xmerl_xpath:string("/html/body/form/input", XmlElt),
  G = fun(Item) ->
    %lager:debug("Items = ~p~n",[Item]),
    case xmerl_xpath:string("/input/@name", Item) of
      [#xmlAttribute{value = Name}] ->
        [#xmlAttribute{value = Value}] = xmerl_xpath:string("/input/@value", Item),
        {Name,Value};
      _ -> []
    end
    end,
  [G(X)|| X<-Items].


save(ReqData,RespData,RespCode) ->
  TxnLog = #txn_log{
    mcht_txn_seq = proplists:get_value(tranId,ReqData)
    , mcht_txn_date = proplists:get_value(tranDate,ReqData)
    , mcht_txn_time = proplists:get_value(tranTime,ReqData)
    , up_merId = proplists:get_value(<<"merId">>,RespData,<<"null">>)
    , up_txnTime = proplists:get_value(<<"txnTime">>,RespData,<<"null">>)
    , up_orderId = proplists:get_value(<<"orderId">>,RespData,<<"null">>)
    , up_txnAmt = proplists:get_value(<<"txnAmt">>,RespData,<<"null">>)
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
      ,proplists:get_value("merId",RespData,<<"null">>)
      ,<<"|">>
      , proplists:get_value("txnTime",RespData,<<"null">>)
      ,<<"|">>
      ,proplists:get_value("orderId",RespData,<<"null">>)
      ,<<"|">>
      ,proplists:get_value("txnAmt",RespData,<<"null">>)
      ,<<"|">>
      ,integer_to_binary(RespCode)
      ,<<"\r\n">>
    ]
    ,[append]) .

 count_results() ->
   QH = qlc:q([X || X <- mnesia:table(txn_log)]),
   F = fun() ->
     qlc:e(QH)
       end,
   {atomic, L} = mnesia:transaction(F),
   Success = fun(X) when is_tuple(X)-> lists:nth(9,tuple_to_list(X)) =:=success end,
   SuccessTimes = lists:filter(Success,L),
   Faile = fun(X) when is_tuple(X)-> lists:nth(9,tuple_to_list(X)) =:=waiting end,
   FaileTimes = lists:filter(Faile,L),
   lager:info("~nSumTimes = ~p ~n  SuccessTimes = ~p ~n  FaileTimes = ~p ~n",[length(L),length(SuccessTimes),length(FaileTimes)]).








