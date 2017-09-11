-module(xfutils).

%% API exports
%% datetime module exports
-export([
  now/0
  , now/1
  , today/0
  , today/1
  , yesterday/0
  , yesterday/1
  , datetime_string_to_timestamp/1
  , parse_date/1
]).
%% web module exports
-export([
  proplist_to_iolist/1
  , proplist_to_binary/1
]).

%% add right yyyy prefix to mmdd
-export([
  prefix_yyyy_2_dtime/1
  , prefix_yyyy_2_dtime/2
  , prefix_yyyy_2_settle_date/1
  , prefix_yyyy_2_settle_date/2
]).

%% txn module exports
-export([get_new_order_id/0]).

%% utils_app export
-export([priv_dir/0
  , priv_dir/1
]).

%% mnesia transform
-export([
  mnesia_dump_to_file/2
  , mnesia_load_from_file/1
]).

%% web
-export([
  only_allow/2
  , post_get_qs/1
  , post_vals_to_string/1
  , post_vals_to_iolist/1
  , parse_post_body/1
  , record_to_proplist/3
  , post/2
]).

%% bin_to_hex
-export([bin_to_hex/1
  , hex_to_bin/1
  , bin_to_base64_lines/1
  , bin_to_pem/1
  , bin_to_pem_rsa/1
]).


%% convert
-export([up_resp_code_2_txn_status/1]).

%% otp
-export([
  child_spec/1
  , child_spec/2
  , parse_options/2
  , sup_restart_strategy/0
  , sup_restart_strategy/1
]).

%% env
-export([
  get_path/1,
  get_path/2,
  get_filename/1,
  get_filename/2,
  app_env_init_for_test/2,
  app_env_init_for_test/0,
  get/1
]).

%% enckey
-export([
  load_private_key/1
  , load_private_key/2
  , load_public_key/1
]).

%% utils_bin
-export([
  trim_space/1
  , trim_space/2
  , remove_space/1

]).
%%====================================================================
%% API functions
%%====================================================================
%% datetime
-spec now() -> list().
now() ->
  utils_datetime:now().

-spec now(Type) -> list() when
  Type :: types:now_options().

now(Type) ->
  utils_datetime:now(Type).

%%--------------------------------------------------------------------
-spec yesterday() -> Date when
  Date :: types:date_format_yyyymmdd().

yesterday() ->
  utils_datetime:yesterday().

-spec yesterday(Fmt) -> Date when
  Fmt :: types:today_format(),
  Date :: types:date_format_yyyymmdd().

yesterday(Type) ->
  utils_datetime:yesterday(Type).

%%--------------------------------------------------------------------
-spec today() -> Date when
  Date :: types:date_format_yyyymmdd().
today() ->
  utils_datetime:today().

-spec today(Fmt) -> Date when
  Fmt :: types:today_format(),
  Date :: types:date_format_yyyymmdd().

today(Type) ->
  utils_datetime:today(Type).

%%--------------------------------------------------------------------
datetime_string_to_timestamp(String) ->
  datetime:datetime_string_to_timestamp(String).
%%--------------------------------------------------------------------
get_new_order_id() ->
  utils_trans:get_new_order_id().

%%--------------------------------------------------------------------
%% web related
-spec proplist_to_iolist(PL) -> iolist() when
  PL :: proplists:proplist().
proplist_to_iolist(PL) when is_list(PL) ->
  xf_proplists_ex:cvt_to_iolist(PL).

-spec proplist_to_binary(PL) -> binary() when
  PL :: proplists:proplist().
proplist_to_binary(PL) when is_list(PL) ->
  xf_proplists_ex:cvt_to_binary(PL).

%%--------------------------------------------------------------------
-spec prefix_yyyy_2_dtime(DTime) -> iolist() when
  DTime :: binary().
prefix_yyyy_2_dtime(DTime) when is_binary(DTime) ->
  utils_datetime:prefix_yyyy_2_dtime(DTime).

-spec prefix_yyyy_2_dtime(DTime, TODAY) -> iolist() when
  DTime :: binary(),
  TODAY :: types:date_format_yyyymmdd().
prefix_yyyy_2_dtime(DTime, Today) when is_binary(DTime), is_binary(Today) ->
  utils_datetime:prefix_yyyy_2_dtime(DTime, Today).

%%--------------------------------------------------------------------
-spec prefix_yyyy_2_settle_date(DTime) -> iolist() when
  DTime :: binary().
prefix_yyyy_2_settle_date(DTime) when is_binary(DTime) ->
  utils_datetime:prefix_yyyy_2_settle_date(DTime).

-spec prefix_yyyy_2_settle_date(DTime, TODAY) -> iolist() when
  DTime :: binary(),
  TODAY :: types:date_format_yyyymmdd().
prefix_yyyy_2_settle_date(DTime, Today) when is_binary(DTime), is_binary(Today) ->
  utils_datetime:prefix_yyyy_2_settle_date(DTime, Today).
%%--------------------------------------------------------------------
parse_date(Date) when is_list(Date) ->
  datetime:parse_date(Date);
parse_date(Date) when is_binary(Date) ->
  datetime:parse_date(binary_to_list(Date)).
%%--------------------------------------------------------------------
priv_dir() ->
  utils_app:priv_dir().

priv_dir(Application) when is_atom(Application) ->
  utils_app:priv_dir(Application).

%%--------------------------------------------------------------------
mnesia_dump_to_file(FileName, Table) ->
  xf_mnesia_transform:dump_to_file(FileName, Table).

mnesia_load_from_file(FileName) ->
  xf_mnesia_transform:load_from_file(FileName).

%%--------------------------------------------------------------------
only_allow(Method, Req) when is_atom(Method) ->
  utils_web:only_allow(Method, Req).

post_get_qs(Req) ->
  utils_web:post_get_qs(Req).

post_vals_to_string(PostVals) ->
  utils_web:post_vals_to_string(PostVals).

post_vals_to_iolist(PostVals) ->
  utils_web:post_vals_to_iolist(PostVals).

parse_post_body(Body) ->
  utils_web:parse_post_body(Body).

record_to_proplist(M, Model, RecordList) ->
  utils_web:record_to_proplist(M, Model, RecordList).

post(Url, PostString) ->
  utils_web:post(Url, PostString).

%%--------------------------------------------------------------------
bin_to_hex(Bin) when is_binary(Bin) ->
  bin_to_hex:bin_to_hex(Bin).

hex_to_bin(Hex) ->
  bin_to_hex:hex_to_bin(Hex).

bin_to_base64_lines(Bin) when is_binary(Bin) ->
  bin_to_hex:bin_to_base64_lines(Bin).

bin_to_pem(Bin) when is_binary(Bin) ->
  bin_to_hex:bin_to_pem(Bin).

bin_to_pem_rsa(Bin) when is_binary(Bin) ->
  bin_to_hex:bin_to_pem_rsa(Bin).
%%--------------------------------------------------------------------
up_resp_code_2_txn_status(undefined) ->
  waiting;
up_resp_code_2_txn_status(RespCode) when is_binary(RespCode) ->
  utils_convert:get_txn_status(RespCode).

%%--------------------------------------------------------------------
%% otp
child_spec(Module) when is_atom(Module) ->
  utils_otp:child_spec(Module).

child_spec(Module, Options) when is_atom(Module),
  (is_list(Options) or is_map(Options) or
    (Options =:= dynamic)
  ) ->
  utils_otp:child_spec(Module, Options).
%%child_spec(Module, OptionMap) when is_atom(Module), is_map(OptionMap) ->
%%  utils_otp:child_spec(Module, OptionMap).

sup_restart_strategy() ->
  utils_otp:sup_restart_strategy().

sup_restart_strategy(Options) ->
  utils_otp:sup_restart_strategy(Options).

parse_options({OptionType, Options} = OptionTuple, Any) when is_atom(OptionType), is_list(Options) ->
  utils_otp:parse_options(OptionTuple, Any).

%%--------------------------------------------------------------------
%% env
get(Key) ->
  utils_env:get(Key).

get_path(Env) ->
  utils_env:get_path(Env).

get_path(Env, Option) ->
  utils_env:get_path(Env, Option).

get_filename(Env) ->
  utils_env:get_filename(Env).

get_filename(App, Env) ->
  utils_env:get_filename(App, Env).

app_env_init_for_test(App, Props) ->
  utils_env:app_env_init_for_test(App, Props).

app_env_init_for_test() ->
  utils_env:app_env_init_for_test().

%%--------------------------------------------------------------------
%% enckey
-spec load_private_key(KeyFileName) -> {ok, RsaKeyInfo} when
  KeyFileName :: string() | binary(),
  RsaKeyInfo :: binary().

load_private_key(KeyFileName)
  when is_binary(KeyFileName) orelse is_list(KeyFileName) ->
  utils_enckey:load_private_key(KeyFileName).

%%--------------------------------------------------------------------
-spec load_private_key(KeyFileName, Pwd) -> {ok, RsaKeyInfo} when
  KeyFileName :: string() | binary(),
  Pwd :: string(),
  RsaKeyInfo :: binary().

load_private_key(KeyFileName, Pwd)
  when (is_binary(KeyFileName) orelse is_list(KeyFileName))
  , is_list(Pwd) ->
  utils_enckey:load_private_key(KeyFileName, Pwd).

%%--------------------------------------------------------------------
-spec load_public_key(KeyFileName) -> {ok, RsaKeyInfo} when
  KeyFileName :: string() | binary(),
  RsaKeyInfo :: binary().
load_public_key(KeyFileName)
  when is_binary(KeyFileName) orelse is_list(KeyFileName) ->
  utils_enckey:load_public_key(KeyFileName).

%%--------------------------------------------------------------------
trim_space(Bin) when is_binary(Bin) ->
  utils_binary:trim_space(Bin).

trim_space(Bin, Option) when is_binary(Bin), is_atom(Option) ->
  utils_binary:trim_space(Bin, Option).

remove_space(Bin) when is_binary(Bin) ->
  utils_binary:remove_space(Bin).
%%====================================================================
%% Internal functions
%%====================================================================
