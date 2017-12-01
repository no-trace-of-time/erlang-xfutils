%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2016 17:23
%%%-------------------------------------------------------------------
-module(t_xfutils).
-author("simonxu").

-include_lib("eunit/include/eunit.hrl").


today_test() ->
  <<Y:4/bytes, M:2/bytes, D:2/bytes>> = xfutils:today(),
  {{Year, Month, Day}, _} = calendar:now_to_datetime(erlang:timestamp()),
  ?assertEqual(binary_to_integer(Y), Year),
  ?assertEqual(binary_to_integer(M), Month),
  ?assertEqual(binary_to_integer(D), Day).

today_mmdd_test() ->
  <<M:2/bytes, D:2/bytes>> = xfutils:today(mmdd),
  {{_Year, Month, Day}, _} = calendar:now_to_datetime(erlang:timestamp()),
  ?assertEqual(binary_to_integer(M), Month),
  ?assertEqual(binary_to_integer(D), Day).

yesterday_test() ->
  <<Y:4/bytes, M:2/bytes, D:2/bytes>> = xfutils:today(),
  Today = {{binary_to_integer(Y)
    , binary_to_integer(M)
    , binary_to_integer(D)
  }, {0, 0, 0}},
  <<Y1:4/bytes, M1:2/bytes, D1:2/bytes>> = xfutils:yesterday(),
  Yesterday = {{binary_to_integer(Y1)
    , binary_to_integer(M1)
    , binary_to_integer(D1)
  }, {0, 0, 0}},
  {DayDiff, {0, 0, 0}} = calendar:time_difference(Today, Yesterday),
  ?assertEqual(DayDiff, -1).

yesterday__mmdd_test() ->
  <<M:2/bytes, D:2/bytes>> = xfutils:today(mmdd),
  Today = {{2016
    , binary_to_integer(M)
    , binary_to_integer(D)
  }, {0, 0, 0}},
  <<M1:2/bytes, D1:2/bytes>> = xfutils:yesterday(mmdd),
  Yesterday = {{2016
    , binary_to_integer(M1)
    , binary_to_integer(D1)
  }, {0, 0, 0}},
  {DayDiff, {0, 0, 0}} = calendar:time_difference(Today, Yesterday),
  ?assertEqual(DayDiff, -1).

now_test() ->
  <<Y:4/bytes, $-, M:2/bytes, $-, D:2/bytes,
    $T, H:2/bytes, $:, Min:2/bytes, $:, Sec:2/bytes, $.,
    _MSec:6/bytes, "+0", _TC:1/bytes, ":00">>
    = list_to_binary(xfutils:now()),

  Y1 = binary_to_integer(Y),
  M1 = binary_to_integer(M),
  D1 = binary_to_integer(D),
  H1 = binary_to_integer(H),
  Min1 = binary_to_integer(Min),
  Sec1 = binary_to_integer(Sec),


  <<Y:4/bytes, M:2/bytes, D:2/bytes>> = xfutils:today(),
  {{Y1, M1, D1}, {H1, Min1, Sec1}} = calendar:local_time().


get_new_order_id_test() ->
  Now = calendar:local_time(),
  <<Y:4/bytes, M:2/bytes, D:2/bytes,
    H:2/bytes, Min:2/bytes, Sec:2/bytes,
    _MSec:6/bytes, _Random:3/bytes>>
    = xfutils:get_new_order_id(),

  Y1 = binary_to_integer(Y),
  M1 = binary_to_integer(M),
  D1 = binary_to_integer(D),
  H1 = binary_to_integer(H),
  Min1 = binary_to_integer(Min),
  Sec1 = binary_to_integer(Sec),

  Now1 = {{Y1, M1, D1}, {H1, Min1, Sec1}},

  ?assertEqual({0, {0, 0, 0}}, calendar:time_difference(Now, Now1)).




proplist_to_binary_test() ->
  PL = [{a, b}, {c, d}],
  ?assertEqual(xfutils:proplist_to_binary(PL), <<"a=b&c=d">>).


prefix_yyyy_2_dtime_test() ->
  ?assertEqual(xfutils:prefix_yyyy_2_dtime(<<"12312345">>, <<"20170101">>), <<"201612312345">>),
  ?assertEqual(xfutils:prefix_yyyy_2_dtime(<<"12311010">>, <<"20161231">>), <<"201612311010">>),
  ?assertEqual(xfutils:prefix_yyyy_2_dtime(<<"03011010">>, <<"20160301">>), <<"201603011010">>),
  ?assertEqual(xfutils:prefix_yyyy_2_dtime(<<"03011010">>, <<"20160302">>), <<"201603011010">>),
  ok.


compress_test() ->
  ?assertEqual(<<120, 156, 51, 52, 132, 1, 0, 10, 145, 1, 235>>, xfutils:deflate(<<"1111111111">>)),
  ?assertEqual(<<"1111111111">>, xfutils:inflate(<<120, 156, 51, 52, 132, 1, 0, 10, 145, 1, 235>>)),
  ok.

		
