%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc 交易相关接口
%%%
%%% @end
%%% Created : 17. Nov 2016 11:23 AM
%%%-------------------------------------------------------------------
-module(utils_trans).
-author("simon").

%% API
-export([
  get_new_order_id/0
]).

%== =
-define(RAND_INDEX, 1000).
-define(RAND_LEN, 3).
-define(RAND_PREFIX, "000").

get_new_order_id() ->
  % TS = erlang:system_time(micro_seconds),
  TString = utils_datetime:now(ts),
  Rand = rand:uniform(?RAND_INDEX),
  %RandBin = << ?RAND_PREFIX, integer_to_binary(Rand) >>,
  RandList = [?RAND_PREFIX, integer_to_list(Rand)],
  RandBin = list_to_binary(RandList),
  RandBinFixLen = binary:part(RandBin, {byte_size(RandBin), -?RAND_LEN}),
  list_to_binary([TString, RandBinFixLen]).