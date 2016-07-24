-module(xfutils).

%% API exports
%% datetime module exports
-export([now/0, now/1,
				 today/0, today/1,
				 yesterday/0, yesterday/1
]).
%% web module exports
-export([proplist_to_iolist/1,
				 proplist_to_binary/1
]).

%% txn module exports
-export([get_new_order_id/0]).

%%====================================================================
%% API functions
%%====================================================================
%% datetime
-spec now() -> list().
now() ->
	datetime:now().

-spec now(Type) -> list() when
		  Type:: types:now_options().

now(Type) ->
	datetime:now(Type).

%%--------------------------------------------------------------------
-spec yesterday() -> Date when
		  Date:: types:date_format_yyyymmdd().

yesterday() ->
	datetime:yesterday().

-spec yesterday(Fmt) -> Date when
		  Fmt :: types:today_format(),
		  Date:: types:date_format_yyyymmdd().

yesterday(Type) ->
	datetime:yesterday(Type).

%%--------------------------------------------------------------------
-spec today() -> Date when
		  Date:: types:date_format_yyyymmdd().
today() ->
	datetime:today().

-spec today(Fmt) -> Date when
		  Fmt :: types:today_format(),
		  Date:: types:date_format_yyyymmdd().

today(Type) ->
	datetime:today(Type).

%%--------------------------------------------------------------------
get_new_order_id() ->
	datetime:get_new_order_id().

%% web related
-spec proplist_to_iolist(PL) -> iolist() when
	PL :: proplists:proplist().
proplist_to_iolist(PL) when is_list(PL) ->
	xf_proplists_ex:cvt_to_iolist(PL).

-spec proplist_to_binary(PL) -> binary() when
	PL :: proplists:proplist().
proplist_to_binary(PL) when is_list(PL) ->
	xf_proplists_ex:cvt_to_binary(PL).


%%====================================================================
%% Internal functions
%%====================================================================
