%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十一月 2017 21:39
%%%-------------------------------------------------------------------
-author("simon").

-define(LARGER_STACKTRACE_1(X),
  lager:error("Error =~p,stacktrace=~ts", [X, iolist_to_binary(lager:pr_stacktrace(erlang:get_stacktrace()))])).
