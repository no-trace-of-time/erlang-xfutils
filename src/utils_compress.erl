%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十一月 2017 16:30
%%%-------------------------------------------------------------------
-module(utils_compress).
-author("simon").

%% API
-export([
  deflate/1
]).


deflate(Bin) when is_binary(Bin) ->
  Z = zlib:open(),
  ok = zlib:deflateInit(Z, default),

  [BinCompressed] = zlib:deflate(Z, Bin, finish),
  zlib:close(Z),
  BinCompressed.
