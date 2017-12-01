-module(xfutils).
-include_lib("mixer/include/mixer.hrl").

%% API exports
%% web module exports
-export([
  proplist_to_iolist/1
  , proplist_to_binary/1

  , app/0
]).

%%--------------------------------------------------------------------
-mixin([
  {utils_datetime, [now/0, now/1, yesterday/0, yesterday/1, today/0, today/1]}
  , {datetime, [datetime_string_to_timestamp/1, parse_date/1]}
  , {utils_trans, [get_new_order_id/0]}
  , {utils_binary, [to_lower/1, to_upper/1]}
  , {utils_compress, [deflate/1, inflate/1]}
  , {utils_datetime, [prefix_yyyy_2_dtime/1, prefix_yyyy_2_dtime/2, prefix_yyyy_2_settle_date/1
    , prefix_yyyy_2_settle_date/2]}
  , {utils_app, [priv_dir/0, priv_dir/1]}
%%  , {xf_mnesia_transform, [mnesia_dump_to_file/2, mnesia_load_from_file/1]}
  , {utils_web, [only_allow/2, post_get_qs/1, post_vals_to_string/1, post_vals_to_iolist/1, parse_post_body/1
    , record_to_proplist/3, post/2, fetch/1, fetch/2]}
  , {bin_to_hex, [bin_to_hex/1, hex_to_bin/1, bin_to_base64_lines/1, bin_to_pem/1, bin_to_pem_rsa/1]}
  , {utils_convert, [up_resp_code_2_txn_status/1, ext_req/2, ext_req/3]}
  , {utils_otp, [child_spec/1, child_spec/2
    , sup_restart_strategy/0, sup_restart_strategy/1, parse_options/2]}
  , {utils_env, [get/1, get_path/1, get_path/2, get_filename/1, get_filename/2
    , app_env_init_for_test/2, app_env_init_for_test/0]}
  , {utils_enckey, [load_private_key/1, load_private_key/2
    , load_public_key/1, load_public_key/2]}
  , {utils_binary, [trim_space/1, trim_space/2, remove_space/1]}
  , {utils_assert, [assert/2, validate/2]}
  , {utils_lager, [cond_lager/5, cond_lager/4]}
]).

%%====================================================================
%% API functions
%%====================================================================
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

%%-----------------
app() ->
  ?MODULE.
%%====================================================================
%% Internal functions
%%====================================================================
