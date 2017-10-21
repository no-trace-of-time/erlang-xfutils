%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2017 20:03
%%%-------------------------------------------------------------------
-module(utils_lager).
-author("simon").

%% API
-export([
  cond_lager/4
  , cond_lager/5
]).

cond_lager(CondKey, Level, Fmt, Vals) when is_atom(Level), is_atom(CondKey), is_list(Fmt), is_list(Vals) ->
  App = xfutils:app(),
  cond_lager(App, CondKey, Level, Fmt, Vals).

cond_lager(App, CondKey, Level, Fmt, Vals)
  when is_atom(App), is_atom(Level), is_atom(CondKey), is_list(Fmt), is_list(Vals) ->
  case application:get_env(App, CondKey) of
    {ok, true} ->
      lager_output(Level, Fmt, Vals);
    _ ->
      %% not found or is false
      %% not output
      ok
  end.

lager_output(error, Fmt, Vals) ->
  lager:error(Fmt, Vals);
lager_output(info, Fmt, Vals) ->
  lager:info(Fmt, Vals);
lager_output(debug, Fmt, Vals) ->
  lager:debug(Fmt, Vals).

