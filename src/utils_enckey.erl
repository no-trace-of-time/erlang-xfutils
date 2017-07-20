%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 七月 2017 21:40
%%%-------------------------------------------------------------------
-module(utils_enckey).
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
  lager:debug("private key = ~p", [RsaKeyInfo]),
  {ok, RsaKeyInfo}.


%%==================================================================
load_public_key(KeyFileName) ->
  {ok, PemBin} = file:read_file(KeyFileName),
  [Certificate] = public_key:pem_decode(PemBin),
  PublicKey = public_key:pem_entry_decode(Certificate),
  lager:debug("public key = ~p", [PublicKey]),
  {ok, PublicKey}.