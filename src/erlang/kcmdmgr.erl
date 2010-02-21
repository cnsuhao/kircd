%% vim: set fileencoding=utf-8 :
-module(kcmdmgr).
-export([exec_cmd/2]).

-include("kerrcode.hrl").

%% PASS <password>
exec_cmd("PASS", User, Args) ->
    if
        length(Args) == 0 ->
            kuser:send_errcode(User, ERR_NEEDMOREPARAMS),
            io:format("bad!~n");
        true ->
            io:format("good!~n")
    end;

%% 
exec_cmd("USER", Args) ->
    io:format("USER~n");

exec_cmd(_, _) ->
    io:format("other~n").

