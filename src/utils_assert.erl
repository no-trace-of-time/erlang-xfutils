%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Mar 2017 3:48 PM
%%%-------------------------------------------------------------------
-module(utils_assert).
-include_lib("eunit/include/eunit.hrl").
-author("simon").

%% API
-export([
  assert/2
  , validate/2
]).

%%-------------------------------------------------------------------
%% assert/2 : check variable is valid, otherwise throw exception
assert(Atom, Var) when is_atom(Atom) ->
  true = validate(Atom, Var).


%%-------------------------------------------------------------------
%% validate/2: check variable is valid, then return true, otherwise return false
validate(yyyymmdd, <<Year:4/bytes, Month:2/bytes, Day:2/bytes>> = Date) when is_binary(Date) ->
  8 =:= byte_size(Date)
    andalso calendar:valid_date(binary_to_integer(Year)
    , binary_to_integer(Month)
    , binary_to_integer(Day));

validate(yyyymmdd, Date) when is_list(Date) ->
  validate(yyyymmdd, list_to_binary(Date));

validate(yyyymmdd, _) ->
  false;

validate(yymmdd, Date) when is_binary(Date) ->
  validate(yyyymmdd, <<"20", Date/binary>>);

validate(yymmdd, Date) when is_list(Date) ->
  validate(yymmdd, list_to_binary(Date));

validate(yymmdd, _) ->
  false;

validate(_, _) ->
  false.

validate_test() ->
  ?assertEqual(true, validate(yyyymmdd, <<"20100101">>)),
  ?assertEqual(true, validate(yyyymmdd, <<"19990909">>)),
  ?assertEqual(true, validate(yyyymmdd, <<"20990909">>)),
  ?assertEqual(false, validate(yyyymmdd, <<"201001">>)),
  ?assertEqual(false, validate(yyyymmdd, <<"20">>)),
  ?assertEqual(false, validate(yyyymmdd, <<"20109999">>)),
  ?assertEqual(false, validate(yyyymmdd, <<"20170229">>)),

  ?assertEqual(true, validate(yymmdd, <<"100101">>)),
  ?assertEqual(true, validate(yymmdd, <<"990909">>)),
  ?assertEqual(false, validate(yymmdd, <<"1001">>)),
  ?assertEqual(false, validate(yymmdd, <<"">>)),

  ?assertEqual(false, validate(yyyymmdd, "20170229")),

  ok.
