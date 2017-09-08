%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 七月 2017 21:45
%%%-------------------------------------------------------------------
-module(utils_otp).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([
  child_spec/1
  , child_spec/2
  , parse_options/2
  , sup_restart_strategy/0
  , sup_restart_strategy/1
]).


child_spec(Module) when is_atom(Module) ->
  child_spec(Module, []).

child_spec(Module, dynamic) ->
  OptionsMap = parse_options({dynamic_child_spec, []}, Module),
  child_spec(Module, OptionsMap);
child_spec(Module, supervisor) ->
  OptionsMap = parse_options({child_spec, [{type, supervisor}]}, Module),
  child_spec(Module, OptionsMap);
child_spec(Module, Options) when is_atom(Module), is_list(Options) ->
  OptionsMap = parse_options({child_spec, Options}, Module),
  child_spec(Module, OptionsMap);

child_spec(Module, OptionsMap) when is_atom(Module), is_map(OptionsMap) ->
  #{id:= Id, start:=Start, start_args:=StartArgs, restart:=Restart, shutdown:= Shutdown, type:=Type, modules:=Modules} = OptionsMap,

  {Id,
    {Start, start_link, StartArgs},
    Restart, Shutdown, Type, Modules}.

child_spec_test() ->
  ?assertEqual({gws_test, {gws_test, start_link, []}, permanent, 2000, worker, [gws_test]},
    child_spec(gws_test)),

  ?assertEqual({gws_test, {gws_test, start_link, []}, temporary, brutal_kill, worker, [gws_test]},
    child_spec(gws_test, dynamic)),

  ?assertEqual({gws_test, {gws_test, start_link, []}, permanent, 2000, supervisor, [gws_test]},
    child_spec(gws_test, supervisor)),
  ok.

%%-------------------------------------------------------------------
-spec parse_options({OptionsType, Options}, any()) -> map() when
  OptionsType :: child_spec | sup_restart_strategy,
  Options :: list().

parse_options({child_spec, Options}, Module) when is_list(Options), is_atom(Module) ->
  DefaultValues = #{id=>Module, start=>Module, start_args=>[], restart=>permanent, shutdown=>2000, type=>worker, modules=>[Module]},
  do_parse_options(Options, DefaultValues);
parse_options({dynamic_child_spec, Options}, Module) when is_list(Options), is_atom(Module) ->
  DefaultValues = #{id=>Module, start=>Module, start_args=>[], restart=>temporary, shutdown=>brutal_kill
    , type=>worker, modules=>[Module]},
  do_parse_options(Options, DefaultValues);
parse_options({dynamic_sup_restart_strategy, Options}, []) when is_list(Options) ->
  DefaultValues = #{strategy=>simple_one_for_one, intensity=>1, period=>60},
  do_parse_options(Options, DefaultValues);
parse_options({sup_restart_strategy, Options}, []) when is_list(Options) ->
  DefaultValues = #{strategy=>one_for_one, intensity=>4, period=>60},
  do_parse_options(Options, DefaultValues).

do_parse_options(Options, DefaultValues) when is_list(Options), is_map(DefaultValues) ->
  F = fun
        ({Key, Value}, Acc) ->
          AccNew = maps:put(Key, Value, Acc),
          AccNew
      end,

  lists:foldl(F, DefaultValues, Options).

parse_options_test() ->
  ?assertEqual(#{id=>gws_test, start=>gws_test, start_args=>[], restart=>permanent,
    shutdown=>2000, type=>worker, modules=>[gws_test]},
    parse_options({child_spec, []}, gws_test)),
  ?assertEqual(#{id=>gws_test, start=>gws_test, start_args=>[], restart=>temporary,
    shutdown=>brutal_kill, type=>worker, modules=>[gws_test]},
    parse_options({dynamic_child_spec, []}, gws_test)),

  ?assertEqual(#{strategy=>one_for_one, intensity=>4, period=>60},
    parse_options({sup_restart_strategy, []}, [])),

  ok.
%%-------------------------------------------------------------------
sup_restart_strategy() ->
  sup_restart_strategy([]).

sup_restart_strategy(dynamic) ->
  OptionsMap = parse_options({dynamic_sup_restart_strategy, []}, []),
  sup_restart_strategy(OptionsMap);
sup_restart_strategy(Options) when is_list(Options) ->
  OptionsMap = parse_options({sup_restart_strategy, Options}, []),
  sup_restart_strategy(OptionsMap);
sup_restart_strategy(OptionsMap) when is_map(OptionsMap) ->
  #{strategy:= Strategy, intensity:=Intensity, period:=Period} = OptionsMap,
  {Strategy, Intensity, Period}.

sup_restart_strategy_test() ->
  ?assertEqual({one_for_one, 4, 60}, sup_restart_strategy()),
  ?assertEqual({simple_one_for_one, 1, 60}, sup_restart_strategy(dynamic)),
  ok.