%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 八月 2017 17:02
%%%-------------------------------------------------------------------

-module(simulator_enc).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").
-author("pingjianwei").

-define(SINGSTR, <<"8c10a1f68d830818fd8a2c611a6eb7a2e082fc13">>).

%% API
-export([]).
-compile(export_all).



get_private_key(KeyFileName, Pwd) ->
  try
    {ok, PemBin} = file:read_file(KeyFileName),
    [RSAEntry | _Rest] = public_key:pem_decode(PemBin),
    RsaKeyInfo = public_key:pem_entry_decode(RSAEntry, Pwd),
%%    {RsaKeyInfo, PemBin}
    RsaKeyInfo
  catch
    error :X ->
      lager:error("read private key file ~p error! Msg = ~p", [KeyFileName, X]),
      {<<>>, <<>>}
  end.

get_public_key(KeyFileName) ->
  try
    {ok, PemBin} = file:read_file(KeyFileName),
    [Certificate] = public_key:pem_decode(PemBin),
    PublicKey = public_key:pem_entry_decode(Certificate),
    {PublicKey, PemBin}
  catch
    error:X ->
      lager:error("read public key file ~p error! Msg = ~p", [KeyFileName, X]),
      {<<>>, <<>>}
  end.


sign_hex(DigestBin, PrivateKey) ->
%%  SignedBin时可读的二进制，如<<"123你好">>
  SignedBin = public_key:sign(DigestBin, sha, PrivateKey),
  Hex = xfutils:bin_to_hex(SignedBin),
  Hex.


%%---------------------------------------------------------------------------------
%% 获取up公私钥，生成签名并验签
%%---------------------------------------------------------------------------------


get_up_private_key(KeyFileName, Pwd) ->
  {ok, PemBin} = file:read_file(KeyFileName),
  [RSAEntry | _Rest] = public_key:pem_decode(PemBin),
  RsaKeyInfo = public_key:pem_entry_decode(RSAEntry, Pwd),
  RsaKey = public_key:der_decode('RSAPrivateKey', RsaKeyInfo#'PrivateKeyInfo'.privateKey),
  RsaKey.

get_up_public_key(PKPath) ->
  {ok, PemBin} = file:read_file(PKPath),
  [Certificate] = public_key:pem_decode(PemBin),
  {_, DerCert, _} = Certificate,
  Decoded = public_key:pkix_decode_cert(DerCert, otp),
  PublicKey = Decoded#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
  PublicKey.

%%Bin是16进制数，并且都是小写
up_sign(Bin, Key) ->
%%  %S = public_key:sign({digest,Bin},'sha',Key),
  S = public_key:sign(Bin, 'sha', Key),
  B = base64:encode(S),
  B.



very() ->
  PrivateKey =get_private_key("src/keys/cfca-key-28-pwd-111111.key", "111111"),
  lager:info("PrivateKey = ~p~n",[PrivateKey]),
  Signature1 = up_sign(?SINGSTR, PrivateKey),
  lager:info("Signature1 = ~p~n",[Signature1]),

  Signature = base64:decode(<<"Rtxifbaz9Tvh8SEgCnbldo6/zaQAvwJMgrGHN/DVabZnJcpb9MmrkAdXv+lmvQ7VwVhCcrbudNmAj9JMtz1WwHQUwUeRsEZPiS/ul47ZhWdHs+6KRBfNSkyqNUABt9Y8Gem18QFTZFV4lojlVRNLPgoNacToN+9/l64YgnzplW3lGZo2kAEHtzxJmTxDyTaBKoGPKNG2DE+esF1u5TpG5SRXXpNgKtK9XdXyQ7ZZO2qsCYhmRx/HLKA2Y84FTKW2Rd9EtsSNKuVytaklP3ku7f7VLvrE3yA8izaoUDkPZzdLy53L/WjbB6pXIMrAJ/ABmvAdMdKgIIoX48hz+JFHhw==">>),
  lager:info("Signature = ~p~n",[Signature]),
  PubulicKey = get_up_public_key("src/keys/acp.pem"),
  public_key:verify(?SINGSTR,sha,Signature,PubulicKey).