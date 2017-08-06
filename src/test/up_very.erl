%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 七月 2017 15:00
%%%-------------------------------------------------------------------
-module(up_very).
-author("pingjianwei").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").
-define(SINGSTR, <<"8c10a1f68d830818fd8a2c611a6eb7a2e082fc13">>).

%% API
-export([]).
-compile(export_all).

get_private_key(KeyFileName, Pwd) ->
  {ok, PemBin} = file:read_file(KeyFileName),
  [RSAEntry | _Rest] = public_key:pem_decode(PemBin),
  RsaKeyInfo = public_key:pem_entry_decode(RSAEntry, Pwd),
  RsaKey = public_key:der_decode('RSAPrivateKey', RsaKeyInfo#'PrivateKeyInfo'.privateKey),
  RsaKey.

up_sign(Bin, Key) ->
  S = public_key:sign(Bin, 'sha', Key),
  S.

public_key() ->
  PKFile = "src/keys/acp.pem",
  {ok, PemBin} = file:read_file(PKFile),
  [Certificate] = public_key:pem_decode(PemBin),
  {_, DerCert, _} = Certificate,
  Decoded = public_key:pkix_decode_cert(DerCert, otp),
  PublicKey = Decoded#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
  PublicKey.

very() ->
  PrivateKey =get_private_key("src/keys/cfca-key-28-pwd-111111.key", "111111"),
  lager:info("PrivateKey = ~p~n",[PrivateKey]),
  Signature1 = up_sign(?SINGSTR, PrivateKey),
  lager:info("Signature1 = ~p~n",[Signature1]),

  Signature = base64:decode(<<"Rtxifbaz9Tvh8SEgCnbldo6/zaQAvwJMgrGHN/DVabZnJcpb9MmrkAdXv+lmvQ7VwVhCcrbudNmAj9JMtz1WwHQUwUeRsEZPiS/ul47ZhWdHs+6KRBfNSkyqNUABt9Y8Gem18QFTZFV4lojlVRNLPgoNacToN+9/l64YgnzplW3lGZo2kAEHtzxJmTxDyTaBKoGPKNG2DE+esF1u5TpG5SRXXpNgKtK9XdXyQ7ZZO2qsCYhmRx/HLKA2Y84FTKW2Rd9EtsSNKuVytaklP3ku7f7VLvrE3yA8izaoUDkPZzdLy53L/WjbB6pXIMrAJ/ABmvAdMdKgIIoX48hz+JFHhw==">>),
  lager:info("Signature = ~p~n",[Signature]),
  PubulicKey = public_key(),
  public_key:verify(?SINGSTR,sha,Signature,PubulicKey).