-module(xfutils).

%% API exports
%% datetime module exports
-export([now/0,now/1,
		 yesterday/0,yesterday/1
		]).

%% txn module exports
-export([get_new_order_id/0]).

%%====================================================================
%% API functions
%%====================================================================
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


%%====================================================================
%% Internal functions
%%====================================================================
