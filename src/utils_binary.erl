%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 九月 2017 23:07
%%%-------------------------------------------------------------------
-module(utils_binary).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([
  remove_space/1
  , trim_space/1
  , trim_space/2
  , to_lower/1
  , to_upper/1
]).

-define(BIN_SPACE, <<" ">>).
-define(BIN_NULL, <<"">>).

%%=============================================================================
rev(Bin) when is_binary(Bin) ->
  S = size(Bin) * 8,
  <<X:S/integer-little>> = Bin,
  <<X:S/integer-big>>.

%%=============================================================================
trim_space_head(<<>>) ->
  <<>>;
trim_space_head(<<$\ , RestBin/binary>> = Bin) when is_binary(Bin) ->
  trim_space_head(RestBin);
trim_space_head(Bin) ->
  Bin.

trim_space_head_test() ->
  ?assertEqual(<<>>, trim_space_head(<<>>)),
  ?assertEqual(<<"abc">>, trim_space_head(<<"  abc">>)),
  ok.
%%=============================================================================
trim_space(Bin) when is_binary(Bin) ->
  trim_space(Bin, head).

trim_space(Bin, head) ->
  trim_space_head(Bin);
trim_space(Bin, all) ->
  trim_space(trim_space_head(Bin), tail);
trim_space(Bin, tail) ->
  Rev = rev(Bin),
  rev(trim_space_head(Rev)).


trim_space_test() ->
  ?assertEqual(<<"a   b   c   ">>, trim_space(<<"   a   b   c   ">>)),
  ?assertEqual(<<"a   b   c   ">>, trim_space(<<"   a   b   c   ">>, head)),
  ?assertEqual(<<"   a   b   c">>, trim_space(<<"   a   b   c   ">>, tail)),
  ?assertEqual(<<"a   b   c">>, trim_space(<<"   a   b   c   ">>, all)),
  ?assertEqual(<<>>, trim_space(<<"            ">>)),
  ok.
%%=============================================================================
remove_space(Bin) when is_binary(Bin) ->
  binary:replace(Bin, ?BIN_SPACE, ?BIN_NULL, [global]).

remove_space_test() ->
  ?assertEqual(<<"abc">>, remove_space(<<"a   b   c   ">>)),
  ok.

%%---------------------------------------------------------------------------------
to_lower_byte(Byte) when ($A =< Byte) andalso (Byte =< $Z) ->
  Byte + ($a - $A);
to_lower_byte(Byte) ->
  Byte.

to_lower_test() ->
  ?assertEqual([$1, $2, $a, $b, $a, $b, $z],
    [to_lower_byte(B) || B <- [$1, $2, $a, $b, $A, $B, $Z]]),

  ?assertEqual(<<"123abcabczzz">>, to_lower(<<"123abcABcZZz">>)),
  ok.


to_lower(Bin) when is_binary(Bin) ->
  <<<<(to_lower_byte(Byte)):8>> || <<Byte:8>>  <= Bin>>.

%%----------------------------------------------------------------------------------
to_upper_byte(Byte) when ($a =< Byte) andalso (Byte =< $z) ->
  Byte - ($a - $A);
to_upper_byte(Byte) ->
  Byte.

to_upper_test() ->
  ?assertEqual([$1, $2, $A, $B, $A, $B, $Z],
    [to_upper_byte(B) || B <- [$1, $2, $a, $b, $A, $B, $Z]]),

  ?assertEqual(<<"123ABCABCZZZ">>, to_upper(<<"123abcABcZZz">>)),
  ok.


to_upper(Bin) when is_binary(Bin) ->
  <<<<(to_upper_byte(Byte)):8>> || <<Byte:8>>  <= Bin>>.
