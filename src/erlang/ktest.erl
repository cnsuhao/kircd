-module(ktest).
-export([mytest/0]).

-include("errcode.hrl").
-include("kuser.hrl").

mytest() ->
    io:format("~p~n", [?ERR_NOSUCHNICK]).
