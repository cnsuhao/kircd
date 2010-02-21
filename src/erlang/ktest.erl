-module(ktest).
-export([mytest/0]).

-include("kuser.hrl").

mytest() ->
    A = #user_record{socket=1,nickname="hello",pass="abc",status=none},
    B = A#user_record{socket=2},
    {A, B}.
