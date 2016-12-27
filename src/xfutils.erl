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
-export([dump_to_file/2]).

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
priv_dir() ->
  utils_app:priv_dir().

priv_dir(Application) when is_atom(Application) ->
  utils_app:priv_dir(Application).

%%--------------------------------------------------------------------
dump_to_file(FileName, Table) ->
  xf_mnesia_transform:dump_to_file(FileName, Table).

%%====================================================================
%% Internal functions
%%====================================================================
