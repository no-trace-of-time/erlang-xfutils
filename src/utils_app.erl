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
-export([priv_dir/0]).

-define(APP,payment_gateway).

-spec priv_dir() -> file:filename().
priv_dir() ->
	Application = case application:get_application() of
									undefined -> ?APP;
									{ok, App} -> App
								end,
	%io:format("Application=~p~n",[Application]),
	code:priv_dir(Application).
