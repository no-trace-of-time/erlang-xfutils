%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc 封装常用的application相关接口
%%%
%%% @end
%%% Created : 13. Apr 2016 15:11
%%%-------------------------------------------------------------------
-module(utils_app).
-author("simonxu").

%% API
-export([priv_dir/0
  , priv_dir/1]).

-define(APP, payment_gateway).

-spec priv_dir() -> file:filename().
priv_dir() ->
  {ok,Application} = application:get_application(),
  priv_dir(Application).

priv_dir(Application) ->
  %io:format("Application=~p~n",[Application]),
  code:priv_dir(Application).
