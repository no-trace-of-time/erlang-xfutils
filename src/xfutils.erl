-module(xfutils).

%% API exports
%% datetime module exports
-export([now/0,now/1,
		 yesterday/0,yesterday/1
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
now() ->
		datetime:now().

now(Type) -> 
		datetime:now(Type).

yesterday() ->
		datetime:yesterday().

yesterday(Type) ->
		datetime:yesterday(Type).

today() ->
		datetime:today().

today(Type) ->
		datetime:today(Type).

get_new_order_id() ->
		datetime:get_new_order_id().

%% web related
-spec proplist_to_iolist(PL) -> iolist() when
		  PL :: proplists:proplist().
proplist_to_iolist(PL) when is_list(PL) ->
		utils_web:post_vals_to_iolist(PL).

-spec proplist_to_binary(PL) -> binary() when
		  PL :: proplists:proplist().
proplist_to_binary(PL) when is_list(PL) ->
		utils_web:post_vals_to_string(PL).


%%====================================================================
%% Internal functions
%%====================================================================
