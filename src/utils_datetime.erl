%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc 日期处理相关常用函数，可以内部再调用datetime输出的函数
%%%
%%% @end
%%% Created : 17. Nov 2016 11:23 AM
%%%-------------------------------------------------------------------
-module(utils_datetime).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([
  now/0
  , now/1
  , today/0
  , yesterday/0
  , today/1
  , yesterday/1
  , prefix_yyyy_2_dtime/1
  , prefix_yyyy_2_dtime/2
  , prefix_yyyy_2_settle_date/1
  , prefix_yyyy_2_settle_date/2
]).
%%--------------------------------------------------------
now() ->
  now(local).

now(epoch) ->
  erlang:system_time(milli_seconds);
now(local) ->
  datetime:now_to_local_string(erlang:timestamp());
now(utc) ->
  datetime:now_to_utc_string(erlang:timestamp());
%% used for up txn req packet
now(txn) ->
  datetime:now_to_local_txn_string(erlang:timestamp());
now(ts) ->
  datetime:now_to_local_ts_string(erlang:timestamp()).

%%--------------------------------------------------------
-spec today() -> Date when
  Date :: types:date_format_yyyymmdd().

today() ->
  datetime:localtime_to_yyyymmdd(datetime:localtime()).

-spec today(Fmt) -> Date when
  Fmt :: types:today_format(),
  Date :: types:date_format_yyyymmdd().

today(mmdd) ->
  YYYYMMDD = today(),
  binary:part(YYYYMMDD, 4, 4).
%%--------------------------------------------------------
-spec yesterday() -> Date when
  Date :: types:date_format_yyyymmdd().

yesterday() ->
  Seconds = datetime:localtime_to_seconds(datetime:localtime()),
  YesterdayTime = calendar:gregorian_seconds_to_datetime(Seconds - 86400),
  datetime:localtime_to_yyyymmdd(YesterdayTime).

-spec yesterday(Fmt) -> Date when
  Fmt :: types:today_format(),
  Date :: types:date_format_yyyymmdd().

yesterday(mmdd) ->
  YYYYMMDD = yesterday(),
  binary:part(YYYYMMDD, 4, 4).

%%--------------------------------------------------------
prefix_yyyy_2_dtime(DTime) when is_binary(DTime) ->
  prefix_yyyy_2_dtime(DTime, today()).

prefix_yyyy_2_dtime(DTime, Today) when is_binary(Today) ->
  <<ThisYear:4/bytes, MMDD_IN_TODAY:4/bytes, _/binary>> = Today,
  <<MMDD:4/bytes, _/binary>> = DTime,
  prefix_yyyy_2_dtime(DTime, MMDD, ThisYear, MMDD_IN_TODAY).

prefix_yyyy_2_dtime(DTime, <<"1231">>, ThisYear, MMDD_IN_TODAY)
  when is_binary(DTime), is_binary(ThisYear), is_binary(MMDD_IN_TODAY) ->

  case binary_to_integer(MMDD_IN_TODAY) < 1231 of
    true ->
      %% should use last year
      %% DTime = 1231 xx:xx  , curr time = YYYY 0101 xx:xx
      %% last year = YYYY-1
      LastYear = integer_to_binary(binary_to_integer(ThisYear) - 1);
    false ->
      LastYear = ThisYear
  end,
  list_to_binary([LastYear, DTime]);

prefix_yyyy_2_dtime(DTime, _, ThisYear, _) when is_binary(DTime), is_binary(ThisYear) ->
  list_to_binary([ThisYear, DTime]).

%%--------------------------------------------------------
prefix_yyyy_2_settle_date(MMDD) when is_binary(MMDD) ->
  prefix_yyyy_2_settle_date(MMDD, today()).

prefix_yyyy_2_settle_date(MMDD, Today) when is_binary(MMDD), is_binary(Today) ->
  <<Year_IN_TODAY:4/bytes, MMDD_IN_TODAY:4/bytes, _/binary>> = Today,
  4 = byte_size(MMDD),

  prefix_yyyy_2_settle_date(MMDD, Year_IN_TODAY, MMDD_IN_TODAY).

prefix_yyyy_2_settle_date(MMDD, Year_IN_TODAY, MMDD_IN_TODAY)
  when is_binary(MMDD), is_binary(Year_IN_TODAY), is_binary(MMDD_IN_TODAY) ->

  SettleYear = case binary_to_integer(MMDD) < binary_to_integer(MMDD_IN_TODAY) of
                 true ->
                   %% settle date less than today, year should be next year
                   integer_to_binary(binary_to_integer(Year_IN_TODAY) + 1);
                 false ->
                   %% settle date large than today, year should be same as today's year
                   Year_IN_TODAY
               end,

  <<SettleYear/binary, MMDD/binary>>.


%%--------------------------------------------------------
prefix_yyyy_2_dtime_test() ->
  DTime = <<"1231">>,

  ?assertEqual(prefix_yyyy_2_dtime(DTime, <<"20160101">>),
    list_to_binary([<<"2015">>, DTime])),
  ?assertEqual(prefix_yyyy_2_dtime(DTime, <<"20161231">>),
    list_to_binary([<<"2016">>, DTime])),

  DTime1 = <<"0301">>,
  ?assertEqual(prefix_yyyy_2_dtime(DTime1, <<"20160301">>),
    list_to_binary([<<"2016">>, DTime1])),
  ?assertEqual(prefix_yyyy_2_dtime(DTime1, <<"20160302">>),
    list_to_binary([<<"2016">>, DTime1])),
  ok.

prefix_yyyy_2_settle_date_test() ->
  ?assertEqual(prefix_yyyy_2_settle_date(<<"0101">>, <<"20161231">>), <<"20170101">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"0101">>, <<"20161230">>), <<"20170101">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"0101">>, <<"20170101">>), <<"20170101">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"0102">>, <<"20170101">>), <<"20170102">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"1231">>, <<"20171230">>), <<"20171231">>),
  ?assertEqual(prefix_yyyy_2_settle_date(<<"1230">>, <<"20171231">>), <<"20181230">>),
  ok.
