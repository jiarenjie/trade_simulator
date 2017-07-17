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

%% API
-export([send_mcht_req/0]).

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
  lager:info("PostString=~p",[PostString]),
  Url = "http://localhost:8888/pg/pay/",
 % RequestResult = httpc:request(post,{Url, [], "application/x-www-form-urlencoded", PostString}, [], []),
  %lager:info("Notify result = ~p~n ,post_vals=~p", [RequestResult, PostVals])

  {ok,{_,_,Body}} = httpc:request(post,{Url, [], "application/x-www-form-urlencoded", PostString}, [], []),
  XmlElt = parse_up_html(Body),
  lager:info("Notify result = ~p~n ", [XmlElt])
  %lager:info("Notify result = ~p~n ", [Body])
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
