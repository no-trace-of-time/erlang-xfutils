%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2016 20:45
%%%-------------------------------------------------------------------
-module(utils_web).
-include_lib("eunit/include/eunit.hrl").
-include("../include/xfutils.hrl").
-author("simonxu").
-compile({no_auto_import, [get/1]}).

%% API
-export([
  only_allow/2
  , post_get_qs/1
  , post_vals_to_string/1
  , post_vals_to_iolist/1
  , parse_post_body/1
  , record_to_proplist/3
  , post/2
  , fetch/2
  , fetch/1
]).

%%---------
%% Usage: check Req, if only allow post/get cond match, then return ok
%%        otherwise send http reply 405 method not allowed
%%-------
only_allow(post, Req) ->
  {Method, Req2} = cowboy_req:method(Req),
%%  lager:debug("Method = ~p~n", [Method]),
  check_method_post(Method, Req2);
only_allow(get, Req) ->
  {Method, Req2} = cowboy_req:method(Req),
  check_method_get(Method, Req2).

%%==================================================================
check_method_post(<<"POST">>, Req) ->
  {ok, Req};
check_method_post(_, Req) ->
  %% Method not allowed.
  {ok, Req2} = cowboy_req:reply(405, Req),
  {shutdown, Req2}.

check_method_get(<<"GET">>, Req) ->
  {ok, Req};
check_method_get(_, Req) ->
  %% Method not allowed.
  {ok, Req2} = cowboy_req:reply(405, Req),
  {shutdown, Req2}.

%%==================================================================
post_get_qs(Req) ->
  HasBody = cowboy_req:has_body(Req),
  get_post_qs(HasBody, Req).

get_post_qs(false, Req) ->
  {PostVals, Req2} = cowboy_req:qs_vals(Req),
  {ok, PostVals, Req2};
get_post_qs(true, Req) ->
  {ok, PostValsInURL, Req1} = get_post_qs(false, Req),
  {ok, PostVals, Req2} = cowboy_req:body_qs(Req1),
  lager:debug("PostValsInURL = ~p~n,PostValsInBody = ~p", [PostValsInURL, PostVals]),
  {ok, PostVals ++ PostValsInURL, Req2}.


%%==================================================================
-spec post_vals_to_string(PostVals) -> binary() when
  PostVals :: proplists:proplist().
post_vals_to_string(PostVals) ->
  StringList = post_vals_to_iolist(PostVals),
  list_to_binary(StringList).

-spec post_vals_to_iolist(PostVals) -> iolist() when
  PostVals :: proplists:proplist().
post_vals_to_iolist(PostVals) ->
  RP = lists:reverse(PostVals),
  {L, _} = post_to_iolist(RP, {[], true}),
  %io:format("L = ~p", [L]),
  L.

post_to_iolist([], {Acc, IsTail}) ->
  {Acc, IsTail};
post_to_iolist([{Key, Value} | Tail], {Acc, IsTail}) ->
  %io:format("KV=~p,Tail = ~p,Acc=~p~n", [{Key, Value}, Tail, Acc]),
  {L, IsTailNew} = kv2string(Key, Value, IsTail),
  post_to_iolist(Tail, {[L | Acc], IsTailNew}).


kv2string(Key, Value, true) ->
  L = kv2string_tail(Key, Value),
  case L of
    [] -> {[], true};
    L -> {L, false}
  end;

kv2string(Key, Value, false) ->
  L = kv2string_tail(Key, Value),
  case L of
    [] -> {[], false};
    L -> {L ++ [<<"&">>], false}
  end.

kv2string_tail(_Key, undefined) ->
  [];
kv2string_tail(_Key, []) ->
  [];
kv2string_tail(_Key, <<>>) ->
  [];
kv2string_tail(Key, Value) ->
  VBin = key2string(Value),
  VBinEncode = http_uri:encode(binary_to_list(VBin)),
  [key2string(Key), <<"=">>, VBinEncode].

key2string(K) when is_atom(K) ->
  atom_to_binary(K, utf8);
key2string(K) when is_list(K) ->
  K;
key2string(K) when is_integer(K) ->
  integer_to_binary(K);
key2string(K) when is_float(K) ->
  float_to_binary(K);
key2string(K) when is_tuple(K) ->
  tuple_to_list(K);
key2string(K) when is_binary(K) ->
  K.


post_vals_to_string_test() ->

  ?assertEqual(post_vals_to_iolist([]), []),
  ?assertEqual(post_vals_to_iolist([{seqId, []}]), [[]]),

  PL = [
    {<<"merId">>, <<"0001">>}
    , {<<"seqId">>, <<"111333">>}
    , {seqId, 1}
    , {queryId, []}
    , {seqId, undefined}
    , {seqId, <<>>}
  ],

  ?assertEqual(post_vals_to_iolist(PL),
    [
      [<<"merId">>, <<"=">>, "0001", <<"&">>]
      , [<<"seqId">>, <<"=">>, "111333", <<"&">>]
      , [<<"seqId">>, <<"=">>, "1"]
      , []
      , []
      , []
    ]),
  ?assertEqual(post_vals_to_string(PL),
    <<"merId=0001&seqId=111333&seqId=1">>),
  ok.


%%==================================================================
record_to_proplist(M, Model, RecordList) ->
  [
    {X, M:get(Model, X)}
    || X <- RecordList

  ].
%%==================================================================
parse_post_body(Body) when is_list(Body) ->
  parse_post_body(list_to_binary(Body));
parse_post_body(Body) when is_binary(Body) ->
  L = binary:split(Body, [<<"&">>], [global]),
  [parse_kv(X)
    || X <- L
  ].

parse_kv(X) when is_binary(X) ->
  %io:format("X=~p~n",[X]),
  [K, V] = binary:split(X, [<<"=">>], [trim_all]),
  K1 = utils_binary:trim_space(K, all),
  V1 = utils_binary:trim_space(V, all),
  {K1, V1}.

parse_post_body_test() ->
  A = <<"accessType=0&bizType=000201&currencyCode=156&encoding=UTF-8&issuerIdentifyMode=0"
  "&merId=898350249000240&orderId=20160424144700524616076&origRespCode=00"
  "&origRespMsg = [0000000]&queryId = 201604241447002051508&reqReserved = "
  "{pI = test,aI=03429500040006212, aN = }&respCode = 00&respMsg = [0000000]&settleAmt = 1"
  "&settleCurrencyCode = 156&settleDate = 0424&signMethod= 01&traceNo = 205150"
  "&traceTime =0424144700&txnAmt = 1&txnSubType = 01&txnTime = 20160424144700&txnType = 01"
  "&version = 5.0.0&certId= 69597475696&signature = n2cHGM83DicNqPFeYGtETl0Pia7F0uxF6yE5F"
  "Jn0Rt/RQRgZ6rL4Z9Xo0I3YDum5hYigCPUSQpkIp99HrZT7t4idXFo5SfOwAPUDoi6BFlaWrJ8CLYqUp0"
  "KIbACPTNxiB1sBWAsBvLbnOoeHFUMKVDgjG0MMXWcTknnvjXHp2fzrAUWUvBpLFi41QfS3X9JXAv6agBL"
  "KHflpTbKDnRsYX8AbaASuD5aJebJ47daex43VBCPTpItCcbK/kOX/NUHSPq8YmxnEYFv84u7hKKdwN + e9"
  "RWCSDsoDzQUVC2wQwpIp3PaO3BQrYxBiW8oZ + XCM53MY6Z5kUMi8Qdd0oDNrAA ==">>,

  B = parse_post_body(A),
  io:format("parse_post_body result = ~p~n", [B]),

  Value = proplists:get_value(<<"accessType">>, B),
  %io:format("Value = ~p~n",[Value]),
  ?assertEqual(Value, <<"0">>),

  ?assertEqual(proplists:get_value(<<"version">>, B), <<"5.0.0">>),

  ok.

%%==================================================================
post(Url, PostString) when is_list(Url) ->
  post(list_to_binary(Url), PostString);
post(Url, PostString) when is_list(PostString) ->
  post(Url, list_to_binary(PostString));
post(Url, PostString) when is_binary(Url), is_binary(PostString) ->
  lager:debug("do http cmd, Url = ~ts,PostString = ~ts", [Url, PostString]),
  {ok, {{_, StatusCode, _}, Headers, Body}} = httpc:request(post,
    {binary_to_list(Url), [], "application/x-www-form-urlencoded", PostString},
    [],
    [{body_format, binary}]
  ),
  lager:info("post StatusCode = ~p,return header = ~p,body=~ts", [StatusCode, Headers, Body]),
  try
    200 = StatusCode,
    {200, Headers, Body}
  catch
    _:X ->
      ?LARGER_STACKTRACE_1(X),
      lager:error("send up req to url ~p error,PostBody = ~ts", [Url, PostString])
  end.
%%==================================================================
fetch(Url) when is_list(Url) ->
  fetch(list_to_binary(Url));
fetch(Url) when is_binary(Url) ->
  fetch(Url, []).

fetch(Url, Params) when is_list(Url), is_list(Params) ->
  fetch(list_to_binary(Url), Params);
fetch(Url, Params) when is_binary(Url), is_list(Params) ->
  lager:debug("do http fetch, Url = ~ts,Params = ~p", [Url, Params]),
  FullUrlBin = <<Url/binary, "?", (list_to_binary(post_vals_to_iolist(Params)))/binary>>,
  lager:debug("FullUrl = ~ts", [FullUrlBin]),
  ?debugFmt("FullUrl = ~ts", [FullUrlBin]),
  {ok, {{_, StatusCode, _}, Headers, Body}} = httpc:request(get,
    {binary_to_list(FullUrlBin), []},
    [
      {ssl,
        [{verify, verify_none}]
      }
    ],
    [{body_format, binary}]
  ),
  lager:info("fetch StatusCode = ~p,return header = ~p,body=~ts", [StatusCode, Headers, Body]),
  try
    200 = StatusCode,
    {200, Headers, Body}
  catch
    _:X ->
      ?LARGER_STACKTRACE_1(X),
      lager:error("send up req to url ~p error,Headers = ~p", [Url, Headers])
  end.
