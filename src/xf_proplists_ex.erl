%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 2016/06/21 18:05
%%%-------------------------------------------------------------------
-module(xf_proplists_ex).
-author("simonxu").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
		 cvt_to_iolist/1
		 ,cvt_to_binary/1
		]).
%%==================================================================
-spec cvt_to_binary(PostVals) -> binary() when
	PostVals :: proplists:proplist().
cvt_to_binary(PostVals) ->
	StringList = cvt_to_iolist(PostVals),
	list_to_binary(StringList).

-spec cvt_to_iolist(PostVals) -> iolist() when
	PostVals :: proplists:proplist().
cvt_to_iolist(PostVals) ->
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


cvt_to_binary_test() ->

	?assertEqual(cvt_to_iolist([]), []),
	?assertEqual(cvt_to_iolist([{seqId, []}]), [[]]),

	PL = [
			{<<"merId">>, <<"0001">>}
		, {<<"seqId">>, <<"111333">>}
		, {seqId, 1}
		, {queryId, []}
		, {seqId, undefined}
		, {seqId, <<>>}
	],

	?assertEqual(cvt_to_iolist(PL),
							 [
									 [<<"merId">>, <<"=">>, "0001", <<"&">>]
								 , [<<"seqId">>, <<"=">>, "111333", <<"&">>]
								 , [<<"seqId">>, <<"=">>, "1"]
								 , []
								 , []
								 , []
							 ]),
	?assertEqual(cvt_to_binary(PL),
							 <<"merId=0001&seqId=111333&seqId=1">>),
	ok.



