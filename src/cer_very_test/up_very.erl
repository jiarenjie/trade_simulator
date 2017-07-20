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
-define(SINGSTR, <<"c527432e8f632d555c651eaf8e5e0b027405fa46">>).

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
  PKFile = "src/keys/certificate.pem",
  {ok, PemBin} = file:read_file(PKFile),
  [Certificate] = public_key:pem_decode(PemBin),
  {_, DerCert, _} = Certificate,
  Decoded = public_key:pkix_decode_cert(DerCert, otp),
  PublicKey = Decoded#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
  PublicKey.

very() ->
  PrivateKey =get_private_key("src/keys/cfca-key-27-pwd-111111.key", "111111"),
  Signature = up_sign(?SINGSTR, PrivateKey),
  PubulicKey = public_key(),
  public_key:verify(?SINGSTR,sha,Signature,PubulicKey).