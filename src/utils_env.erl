%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Dec 2016 5:36 PM
%%%-------------------------------------------------------------------
-module(utils_env).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([get_path/1,
  get_path/2,
  get_filename/1,
  get_filename/2,
%%  app_env_init_for_test/0,
  app_env_init_for_test/2,
  get/1
]).

%%-define(DEFAULT_APP, payment_gateway).

get(Key) ->
  case application:get_env(Key) of
    undefined ->
%%      case application:get_env(?DEFAULT_APP, Key) of
%%        undefined ->
      undefined;
%%        {ok, Value} ->
%%          Value
%%  end;
    {ok, Value} ->
      Value
  end.

app_env_init_for_test(App, Props) when is_atom(App), is_list(Props) ->
%%  App = payment_gateway,
%%  application:set_env(App, priv_dir, "/priv"),
%%  application:set_env(App, mcht_keys_dir, "/keys/mcht"),
%%  application:set_env(App, up_keys_dir, "/keys"),
%%  application:set_env(App, db_backup_dir, "/backup.db/"),
%%  ok.
  F = fun
        ({Key, Value}) ->
          application:set_env(App, Key, Value)
      end,
  lists:map(F, Props).


%%-------------------------------------------------------------------
%%get_path(Env, trim) ->
%%  get_filename(Env).

get_path(home) ->
  {ok, Path} = init:get_argument(home),
  Path;
get_path(Env) when is_atom(Env) ->
%%  case application:get_env(Env) of
%%    undefined ->
  {ok, Path} = application:get_env(Env),
%%      Path;
%%    {ok, Path} ->
  Path;
%%  end;
get_path(EnvList) when is_list(EnvList) ->
  Path = [[get_path(Item), "/"] || Item <- EnvList],
  lists:flatten(Path).

get_path(App, home) when is_atom(App) ->
  get_path(home);
get_path(App, Key) when is_atom(App), is_atom(Key) ->
  {ok, Path} = application:get_env(App, Key),
  Path;
get_path(App, KeyList) when is_atom(App), is_list(KeyList) ->
  Path = [[get_path(App, Item), "/"] || Item <- KeyList],
  lists:flatten(Path).

%%-------------------------------------------------------------------
get_filename(Env) ->
  Path = get_path(Env),
  droplast(Path, $/).

get_filename(App, Env) ->
  Path = get_path(App, Env),
  droplast(Path, $/).

droplast(String, Char) when is_list(String), is_integer(Char) ->
  StringReverse = lists:reverse(String),

  TrimString = trim_head(StringReverse, Char),

  lists:reverse(TrimString).

trim_head([Char | Rest], Char) ->
  trim_head(Rest, Char);
trim_head(Rest, _Char) ->
  Rest.

%%-------------------------------------------------------------------
get_file_test() ->
  ?assertEqual("/ab/c", droplast("/ab/c/", $/)),
  ?assertEqual("/ab/c", droplast("/ab/c", $/)),
  ok.

get_path_test() ->
  App = payment_gateway,
  application:set_env(App, test1, "/aaa/bbb"),
  application:set_env(App, test2, "/ccc/ddd"),
  application:set_env(App, file1, "filename"),
  application:set_env(App, file2, "filename2/"),

  ?assertEqual("/aaa/bbb", get_path(App, test1)),
  ?assertEqual("/ccc/ddd", get_path(App, test2)),
  ?assertEqual("filename", get_path(App, file1)),

  ?assertEqual("/aaa/bbb//ccc/ddd/", get_path(App, [test1, test2])),
  ?assertEqual("/ccc/ddd//aaa/bbb/", get_path(App, [test2, test1])),

  ?assertEqual("/aaa/bbb/filename2", get_filename(App, [test1, file2])),

  ?assertEqual("/aaa/bbb//ccc/ddd", get_filename(App, [test1, test2])),
  ?assertEqual("/ccc/ddd//aaa/bbb", get_filename(App, [test2, test1])),

  ok.

