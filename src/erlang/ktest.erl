-module(ktest).
-export([mytest/0]).

mytest() ->
    A = "abc",
    B = string:len(A),
    if B > 1 ->
        io:format("good~n")
    end.
