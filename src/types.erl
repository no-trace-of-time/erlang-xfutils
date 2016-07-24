-module(types).

-type byte1() :: <<_:8>>.
-type byte2() :: <<_:16>>.
-type byte3() :: <<_:24>>.
-type byte4() :: <<_:32>>.
-type byte5() :: <<_:40>>.
-type byte5_up() :: <<_:40,_:_ * 8>>.
-type byte6() :: <<_:48>>.
-type byte8() :: <<_:64>>.
-type byte8_up() :: <<_:64,_:_ * 8>>.
-type byte14() :: <<_:112>>.
-type byte15() :: <<_:120>>.
-type byte23() :: <<_:184>>.
-type byte23_up() :: <<_:184,_:_ * 8>>.
-type bytes() :: <<_:8,_:_ * 8>>.

-export_type([byte1/0
,byte2/0
,byte3/0
,byte4/0
,byte5/0
,byte5_up/0
,byte6/0
,byte8/0
,byte8_up/0
,byte14/0
,byte15/0
,byte23/0
,byte23_up/0
,bytes/0
]).


-type today_format() :: mmdd.
-type date_format_yyyymmdd() :: types:byte8().
-type date_format_mmdd() :: types:byte4().
-type date_binary() :: date_format_yyyymmdd() | date_format_mmdd().

-export_type([today_format/0
			  ,date_format_yyyymmdd/0
			  ,date_format_mmdd/0
			  ,date_binary/0
			 ]).

-type now_options() :: local | utc | txn | ts .
-export_type([now_options/0]).
