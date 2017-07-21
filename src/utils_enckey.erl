%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 七月 2017 21:40
%%%-------------------------------------------------------------------
-module(utils_enckey).
-include_lib("public_key/include/public_key.hrl").
-author("simon").

%% API
-export([
  load_private_key/1
  , load_private_key/2
  , load_public_key/1
]).

%%==================================================================
load_private_key(KeyFileName) ->
  load_private_key(KeyFileName, "").

load_private_key(KeyFileName, Pwd) ->
  {ok, PemBin} = file:read_file(KeyFileName),
  [RSAEntry | _Rest] = public_key:pem_decode(PemBin),
  RsaKeyInfo = public_key:pem_entry_decode(RSAEntry, Pwd),
  RsaKey = public_key:der_decode('RSAPrivateKey', RsaKeyInfo#'PrivateKeyInfo'.privateKey),
  lager:debug("private key = ~p", [RsaKeyInfo]),
  RsaKey.
%%  lager:debug("private key = ~p", [RsaKeyInfo]),
%%  {ok, RsaKeyInfo}.


%%==================================================================
load_public_key(KeyFileName) ->
  {ok, PemBin} = file:read_file(KeyFileName),
  [Certificate] = public_key:pem_decode(PemBin),
  {_, DerCert, _} = Certificate,
  Decoded = public_key:pkix_decode_cert(DerCert, otp),
  PublicKey = Decoded#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
  lager:debug("public key = ~p", [PublicKey]),
  PublicKey.
%%  PublicKey = public_key:pem_entry_decode(Certificate),
%%  lager:debug("public key = ~p", [PublicKey]),
%%  {ok, PublicKey}.