%%%-------------------------------------------------------------------
%%% @author simonxu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Mar 2016 5:15 PM
%%%-------------------------------------------------------------------
-module(bin_to_hex).
-include_lib("eunit/include/eunit.hrl").

-compile([native, {hipe, [o3]}]).

-export([bin_to_hex/1
  , hex_to_bin/1
  , bin_to_base64_lines/1
  , bin_to_pem/1
  , bin_to_pem_rsa/1
]).

bin_to_hex(B) when is_binary(B) ->
  bin_to_hex(B, <<>>).

-define(H(X), (hex(X)):16).

bin_to_hex(<<>>, Acc) -> Acc;
bin_to_hex(Bin, Acc) when byte_size(Bin) band 7 =:= 0 ->
  bin_to_hex_(Bin, Acc);
bin_to_hex(<<X:8, Rest/binary>>, Acc) ->
  bin_to_hex(Rest, <<Acc/binary, ?H(X)>>).

bin_to_hex_(<<>>, Acc) -> Acc;
bin_to_hex_(<<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>>, Acc) ->
  bin_to_hex_(
    Rest,
    <<Acc/binary,
      ?H(A), ?H(B), ?H(C), ?H(D), ?H(E), ?H(F), ?H(G), ?H(H)>>).

-compile({inline, [hex/1]}).

hex(X) ->
  element(
    X + 1, {16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036,
      16#3037, 16#3038, 16#3039, 16#3041, 16#3042, 16#3043, 16#3044,
      16#3045, 16#3046, 16#3130, 16#3131, 16#3132, 16#3133, 16#3134,
      16#3135, 16#3136, 16#3137, 16#3138, 16#3139, 16#3141, 16#3142,
      16#3143, 16#3144, 16#3145, 16#3146, 16#3230, 16#3231, 16#3232,
      16#3233, 16#3234, 16#3235, 16#3236, 16#3237, 16#3238, 16#3239,
      16#3241, 16#3242, 16#3243, 16#3244, 16#3245, 16#3246, 16#3330,
      16#3331, 16#3332, 16#3333, 16#3334, 16#3335, 16#3336, 16#3337,
      16#3338, 16#3339, 16#3341, 16#3342, 16#3343, 16#3344, 16#3345,
      16#3346, 16#3430, 16#3431, 16#3432, 16#3433, 16#3434, 16#3435,
      16#3436, 16#3437, 16#3438, 16#3439, 16#3441, 16#3442, 16#3443,
      16#3444, 16#3445, 16#3446, 16#3530, 16#3531, 16#3532, 16#3533,
      16#3534, 16#3535, 16#3536, 16#3537, 16#3538, 16#3539, 16#3541,
      16#3542, 16#3543, 16#3544, 16#3545, 16#3546, 16#3630, 16#3631,
      16#3632, 16#3633, 16#3634, 16#3635, 16#3636, 16#3637, 16#3638,
      16#3639, 16#3641, 16#3642, 16#3643, 16#3644, 16#3645, 16#3646,
      16#3730, 16#3731, 16#3732, 16#3733, 16#3734, 16#3735, 16#3736,
      16#3737, 16#3738, 16#3739, 16#3741, 16#3742, 16#3743, 16#3744,
      16#3745, 16#3746, 16#3830, 16#3831, 16#3832, 16#3833, 16#3834,
      16#3835, 16#3836, 16#3837, 16#3838, 16#3839, 16#3841, 16#3842,
      16#3843, 16#3844, 16#3845, 16#3846, 16#3930, 16#3931, 16#3932,
      16#3933, 16#3934, 16#3935, 16#3936, 16#3937, 16#3938, 16#3939,
      16#3941, 16#3942, 16#3943, 16#3944, 16#3945, 16#3946, 16#4130,
      16#4131, 16#4132, 16#4133, 16#4134, 16#4135, 16#4136, 16#4137,
      16#4138, 16#4139, 16#4141, 16#4142, 16#4143, 16#4144, 16#4145,
      16#4146, 16#4230, 16#4231, 16#4232, 16#4233, 16#4234, 16#4235,
      16#4236, 16#4237, 16#4238, 16#4239, 16#4241, 16#4242, 16#4243,
      16#4244, 16#4245, 16#4246, 16#4330, 16#4331, 16#4332, 16#4333,
      16#4334, 16#4335, 16#4336, 16#4337, 16#4338, 16#4339, 16#4341,
      16#4342, 16#4343, 16#4344, 16#4345, 16#4346, 16#4430, 16#4431,
      16#4432, 16#4433, 16#4434, 16#4435, 16#4436, 16#4437, 16#4438,
      16#4439, 16#4441, 16#4442, 16#4443, 16#4444, 16#4445, 16#4446,
      16#4530, 16#4531, 16#4532, 16#4533, 16#4534, 16#4535, 16#4536,
      16#4537, 16#4538, 16#4539, 16#4541, 16#4542, 16#4543, 16#4544,
      16#4545, 16#4546, 16#4630, 16#4631, 16#4632, 16#4633, 16#4634,
      16#4635, 16#4636, 16#4637, 16#4638, 16#4639, 16#4641, 16#4642,
      16#4643, 16#4644, 16#4645, 16#4646}).



hex_to_bin(S) when is_binary(S) ->
  hex_to_bin(binary_to_list(S), []).
hex_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hex_to_bin([X, Y | T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X, Y]),
  hex_to_bin(T, [V | Acc]).


hex_bin_test() ->
  A = <<16#12, 16#34, 16#56, 16#78, 16#90, 16#ab, 16#cd, 16#ef>>,
  H = bin_to_hex(A),
  ?assertEqual(H, <<"1234567890ABCDEF">>),
  B = hex_to_bin(H),
  ?assertEqual(B, A),
  ok.


bin_to_base64_lines(BinHex) when is_binary(BinHex) ->
  Bin = hex_to_bin(BinHex),
  BinBase64 = base64:encode(Bin),
  L = <<<<X:64/bytes, $\n>> || <<X:64/bytes>> <= BinBase64>>,
  lager:debug("L = ~p", [L]),
  %% LenL64 = byte_size(L) div 64,
  % LenL64 = length(L),
  LenRest = byte_size(BinBase64) rem 64,
  lager:debug("lenRest = ~p", [LenRest]),
  Rest = binary:part(BinBase64, byte_size(BinBase64), -LenRest),
  lager:debug("Rest = ~p", [Rest]),
  Return = $\n,
  BinPemPK = <<
    L/binary,
    Rest/binary, $\n
  >>,
  lager:debug("BinPemPK = ~p", [BinPemPK]),

  BinPemPK.

bin_to_pem(BinHex) when is_binary(BinHex) ->
  BinBase64Lines = bin_to_base64_lines(BinHex),

  Return = $\n,
  <<
    "-----BEGIN PUBLIC KEY-----", Return,
    BinBase64Lines/binary,
    "-----END PUBLIC KEY-----", Return
  >>.

bin_to_pem_rsa(BinHex) when is_binary(BinHex) ->
  BinBase64Lines = bin_to_base64_lines(BinHex),

  Return = $\n,
  <<
    "-----BEGIN RSA PUBLIC KEY-----", Return,
    BinBase64Lines/binary,
    "-----END RSA PUBLIC KEY-----", Return
  >>.

bin_to_perm_test() ->
  B = <<"30820122300D06092A864886F70D01010105000382010F003082010A0282010100C4880BD2B22F8200089C5DD7C5CB6004BE1FDDEF833EA14072EDC780C9A6954CCB53F958C76B6E13559FAB36C5D8A3E0F663C4CC09F02ABFA1902F371A9B29222D5CB4F1332B46656FA9073D1F83EE59183C81565AEE459710E0D2CD22E767345A53B677F00C2EF8B3417BF20E3152BA017C36E8F805202EDDF9610961A6D329C83ABBC1B16FA51381C0AAA5EB65C08EB1197CD6977B74EB3FD76E18CC185D3DE3896374FFCA862E9DB4831DE3F89160BA93B1E189F350A24099DEEA22C09EE88FF705FC77A0E62DDB57D9FEFAE081AFB74D6F399E4D065FC5557C46935CB41BFB18F2B5782FA74EE5F6997DA706280220D67B9C7D0EE624D68E1D80F3B588070203010001">>,
  Pem = bin_to_pem(B),
  PemExpected = <<"-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAxIgL0rIvggAInF3Xxctg
BL4f3e+DPqFAcu3HgMmmlUzLU/lYx2tuE1WfqzbF2KPg9mPEzAnwKr+hkC83Gpsp
Ii1ctPEzK0Zlb6kHPR+D7lkYPIFWWu5FlxDg0s0i52c0WlO2d/AMLvizQXvyDjFS
ugF8Nuj4BSAu3flhCWGm0ynIOrvBsW+lE4HAqqXrZcCOsRl81pd7dOs/124YzBhd
PeOJY3T/yoYunbSDHeP4kWC6k7HhifNQokCZ3uoiwJ7oj/cF/Heg5i3bV9n++uCB
r7dNbzmeTQZfxVV8RpNctBv7GPK1eC+nTuX2mX2nBigCINZ7nH0O5iTWjh2A87WI
BwIDAQAB
-----END PUBLIC KEY-----
">>,
  ?assertEqual(Pem, PemExpected).

