%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc 常用的转换功能函数
%%%
%%% @end
%%% Created : 21. Mar 2016 21:11
%%%-------------------------------------------------------------------
-module(utils_convert).
-include_lib("eunit/include/eunit.hrl").
-author("simonxu").

%% API
-export([
  b2l/1
  , b2i/1
  , a2b/1
  , ext_req/2
  , ext_req/3
  , req_fld_ext/2
  , try_ext_req/2
  , exception_string/1
  , pretty_exception_info/2
  , get_txn_status/1
  , trans_key/1
  , trans_value/2
  , mcht_post_field_name_map/0
  , exception_resp_cd/1
]).



%%=============================================================================
-spec b2l(binary()) -> list().
b2l(Bin) ->
  binary_to_list(Bin).

-spec b2i(binary()) -> integer().
b2i(Bin) ->
  binary_to_integer(Bin).

-spec a2b(atom()) -> binary().
a2b(A) ->
  atom_to_binary(A, utf8).

%%=============================================================================
-spec ext_req(Key, Req) -> {any(), any()} when
  Key :: term(),
  Req :: proplists:proplist().
ext_req(Key, Req) ->
  case proplists:lookup(Key, Req) of
    none -> {Key, <<>>};
    Tuple -> Tuple
  end.

-spec ext_req(Key, Req, DefaultValue) -> {any(), any()} when
  Key :: term(),
  Req :: proplists:proplist(),
  DefaultValue :: any().
ext_req(Key, Req, DefaultValue) ->
  case proplists:lookup(Key, Req) of
    none -> {Key, DefaultValue};
    Tuple -> Tuple
  end.

-spec try_ext_req(Key, Req) -> {any(), any()} when
  Key :: term(),
  Req :: proplists:proplist().
try_ext_req(Key, Req) ->
  case proplists:lookup(Key, Req) of
    none -> throw({absence_field, Key});
    {Key} -> throw({field_value_unset, Key});
    {Key, <<>>} -> throw({field_value_empty, Key});
    {Key, Value} -> {Key, Value}
  end.

req_fld_ext(FieldName, Req) ->
  try_ext_req(FieldName, Req).

%%=============================================================================
exception_string(mcht_id_invalid) ->
  <<"商户号不存在"/utf8>>;
exception_string(up_txn_id_duplicate) ->
  <<"支付交易流水号重复"/utf8>>;
exception_string(mcht_txn_id_duplicate) ->
  <<"商户流水号重复"/utf8>>;
exception_string(absence_field) ->
  <<"参数不存在"/utf8>>;
exception_string(field_value_unset) ->
  <<"参数值未设置"/utf8>>;
exception_string(field_value_empty) ->
  <<"参数为空"/utf8>>;
exception_string(sig_verify_failed) ->
  <<"签名验证失败"/utf8>>;
exception_string(up_query_sig_verify_failed) ->
  <<"银联在线交易查询响应签名验证失败"/utf8>>;
exception_string(query_txn_call_failed) ->
  <<"银联在线交易状态查询失败"/utf8>>;
exception_string(query_txn_call_return_code_non_success) ->
  <<"银联在线交易状态查询返回错误码"/utf8>>;
exception_string(mcht_query_txn_seq_not_exist) ->
  <<"商户发送查询流水号查无交易"/utf8>>;
exception_string(mcht_refund_orig_txn_seq_not_exist) ->
  <<"商户发送退货流水号查无交易"/utf8>>;
exception_string(mcht_refund_orig_txn_status_not_success) ->
  <<"商户发送退货交易原支付交易不成功"/utf8>>;
exception_string(out_of_quota) ->
  <<"交易金额超限"/utf8>>;
exception_string(_) ->
  <<"未知错误"/utf8>>.

pretty_exception_info(up_txn_id_duplicate = Reason, {mcht_index_key, {MchtId, TxnDate, TxnSeq}}) when
  is_binary(MchtId), is_binary(TxnDate), is_binary(TxnSeq) ->
  Info = io_lib:format("商户号:~ts,交易日期:~ts,交易流水号:~ts", [MchtId, TxnDate, TxnSeq]),
  full_info(Reason, Info);
pretty_exception_info(up_txn_id_duplicate = Reason, {up_index_key, {MerId, TxnTime, OrderId}}) when
  is_binary(MerId), is_binary(TxnTime), is_binary(OrderId) ->
  Info = io_lib:format("商户号:~ts,交易时间:~ts,支付流水号:~ts", [MerId, TxnTime, OrderId]),
  full_info(Reason, Info);
pretty_exception_info(mcht_txn_id_duplicate = Reason, {MchtId, TxnDate, TxnSeq}) when
  is_binary(MchtId), is_binary(TxnDate), is_binary(TxnSeq) ->
  Info = io_lib:format("商户号:~ts,交易日期:~ts,交易流水号:~ts", [MchtId, TxnDate, TxnSeq]),
  full_info(Reason, Info);
pretty_exception_info(absence_field = Reason, Field) when
  is_binary(Field) ->
  Info = io_lib:format("参数:~p", [Field]),
  full_info(Reason, Info);
pretty_exception_info(field_value_unset = Reason, Field) when
  is_binary(Field) ->
  Info = io_lib:format("参数:~p", [Field]),
  full_info(Reason, Info);
pretty_exception_info(field_value_empty = Reason, Field) when
  is_binary(Field) ->
  Info = io_lib:format("参数:~p", [Field]),
  full_info(Reason, Info);
pretty_exception_info(sig_verify_failed = Reason, {MchtId, TxnDate, TxnSeq, SignString, Sig}) when
  is_binary(MchtId), is_binary(TxnDate), is_binary(TxnSeq)
  , is_binary(SignString), is_binary(Sig) ->
  Info = io_lib:format("商户号:~ts,交易日期:~ts,交易流水号:~ts"
  "签名串:~ts,签名:~ts",
    [(MchtId),
      (TxnDate),
      (TxnSeq), (SignString),
      (Sig)]),
  lager:debug("Info=[~p]", [Info]),
  full_info(Reason, Info);
pretty_exception_info(up_query_sig_verify_failed = Reason, UpRespQuery) ->
  Info = io_lib:format("银联在线交易查询响应内容=~p", [model_up_resp_query:pr(UpRespQuery)]),
  lager:debug("Info=[~p]", [Info]),
  full_info(Reason, Info);
pretty_exception_info(query_txn_call_failed = Reason, {StatusCode, Body}) when
  is_integer(StatusCode), is_binary(Body) ->
  Info = io_lib:format("银联返回http请求状态码=~p,返回错误信息=[~ts]",
    [StatusCode, Body]),
  lager:debug("Info=[~p]", [Info]),
  full_info(Reason, Info);
pretty_exception_info(query_txn_call_return_code_non_success = Reason, UpRespQuery) ->
  Info = io_lib:format("查询请求返回内容=~p",
    [model_up_resp_query:pr(UpRespQuery)]),
  lager:debug("Info=[~p]", [Info]),
  full_info(Reason, Info);
pretty_exception_info(mcht_query_txn_seq_not_exist = Reason, MchtReqQuery) ->
  Info = io_lib:format("商户交易查询内容=~p",
    [model_mcht_req_query:pr(MchtReqQuery)]),
  lager:debug("Info=[~p]", [Info]),
  full_info(Reason, Info);
pretty_exception_info(mcht_refund_orig_txn_seq_not_exist = Reason, MchtReqRefund) ->
  Info = io_lib:format("商户退货交易内容=~p",
    [model_mcht_req_refund:pr(MchtReqRefund)]),
  lager:debug("Info=[~p]", [Info]),
  full_info(Reason, Info);
pretty_exception_info(mcht_refund_amt_greater_than_orig_pay_txn = Reason, MchtReqRefund) ->
  Info = io_lib:format("商户退货交易内容=~p",
    [model_mcht_req_refund:pr(MchtReqRefund)]),
  lager:debug("Info=[~p]", [Info]),
  full_info(Reason, Info);
pretty_exception_info(mcht_id_invalid = Reason, MchtId) ->
  Info = io_lib:format("不存在的商户号=~p", [MchtId]),
  lager:debug("Info=[~p]", [Info]),
  full_info(Reason, Info);
pretty_exception_info(out_of_quota = Reason, TxnInfo) when is_tuple(TxnInfo) ->
  Info = io_lib:format("交易金额超限=~p", [TxnInfo]),
  lager:debug("Info=[~p]", [Info]),
  full_info(Reason, Info);
pretty_exception_info(_, Field) ->
  Info = io_lib:format("~p", [Field]),
  full_info(other, Info).


pretty_exception_info_test() ->
  pretty_exception_info(field_value_empty, <<"Amt">>),
  pretty_exception_info(sig_verify_failed, {<<"000001">>, <<"20121212">>, <<"19999">>, <<"11">>, <<"11">>}),
  ok.

full_info(Reason, Info) ->
  unicode:characters_to_binary([Info, exception_string(Reason)]).
%list_to_binary(io_lib:format("~ts~p",[exception_string(Reason),Info])).

exception_resp_cd(mcht_id_invalid) ->
  <<"31">>;
exception_resp_cd(up_txn_id_duplicate) ->
  <<"12">>;
exception_resp_cd(mcht_txn_id_duplicate) ->
  <<"12">>;
exception_resp_cd(absence_field) ->
  <<"13">>;
exception_resp_cd(field_value_unset) ->
  <<"13">>;
exception_resp_cd(field_value_empty) ->
  <<"13">>;
exception_resp_cd(sig_verify_failed) ->
  <<"11">>;
exception_resp_cd(up_query_sig_verify_failed) ->
  <<"11">>;
exception_resp_cd(query_txn_call_failed) ->
  <<"06">>;
exception_resp_cd(query_txn_call_return_code_non_success) ->
  <<"06">>;
exception_resp_cd(mcht_query_txn_seq_not_exist) ->
  <<"35">>;
exception_resp_cd(mcht_refund_orig_txn_seq_not_exist) ->
  <<"35">>;
exception_resp_cd(mcht_refund_orig_txn_status_not_success) ->
  <<"35">>;
exception_resp_cd(_) ->
  <<"99">>.


%%=============================================================================
-spec get_txn_status(RespCd) -> Status when
  RespCd :: model_up_req_pay:up_respCode(),
  Status :: repo_up_txn_log:txn_status().

get_txn_status(<<"00">>) ->
  success;
get_txn_status(<<"A6">>) ->
  success;
get_txn_status(<<"03">>) ->
  waiting;
get_txn_status(<<"04">>) ->
  waiting;
get_txn_status(<<"05">>) ->
  waiting;
get_txn_status(_) ->
  fail.


%%=============================================================================

trans_key(<<"tranAmt">>) ->
  mcht_txn_amt;
trans_key(<<"settleDate">>) ->
  settle_date;
trans_key(<<"origRespCode">>) ->
  orig_resp_code;
trans_key(<<"origRespMsg">>) ->
  orig_resp_msg;
trans_key(<<"quota">>) ->
  quota;
trans_key(<<"respCode">>) ->
  resp_code;
trans_key(<<"respMsg">>) ->
  resp_msg;
trans_key(<<"merchId">>) ->
  mcht_id;
trans_key(<<"tranDate">>) ->
  mcht_txn_date;
trans_key(<<"tranId">>) ->
  mcht_txn_seq;
trans_key(<<"tranTime">>) ->
  mcht_txn_time;
trans_key(<<"origTranDate">>) ->
  orig_mcht_txn_date;
trans_key(<<"origTranId">>) ->
  orig_mcht_txn_seq;
trans_key(<<"queryId">>) ->
  query_id;
trans_key(<<"origQueryId">>) ->
  orig_query_id;
trans_key(<<"trustBackUrl">>) ->
  mcht_back_url;
trans_key(<<"trustFrontUrl">>) ->
  mcht_front_url;
trans_key(<<"signature">>) ->
  signature.
%%=============================================================================
trans_value(<<"tranAmt">>, V) when is_binary(V) ->
  binary_to_integer(V);
trans_value(_K, V) when is_binary(V) ->
  V.
%%=============================================================================
mcht_post_field_name_map() ->
  #{
    mcht_id => <<"merchId">>
    , mcht_txn_date => <<"tranDate">>
    , mcht_txn_time => <<"tranTime">>
    , mcht_txn_seq => <<"tranId">>
    , query_id => <<"queryId">>
    , mcht_txn_amt => <<"tranAmt">>
    , orig_mcht_txn_date => <<"origTranDate">>
    , orig_mcht_txn_seq => <<"origTranId">>
    , orig_query_id => <<"origQueryId">>
    , settle_date => <<"settleDate">>
    , orig_resp_code => <<"origRespCode">>
    , orig_resp_msg => <<"origRespMsg">>
    , mcht_back_url => <<"trustBackUrl">>
    , mcht_front_url => <<"trustFrontUrl">>
    , quota => <<"quota">>
    , resp_code => <<"respCode">>
    , resp_msg => <<"respMsg">>
    , signature => <<"signature">>
  }.
