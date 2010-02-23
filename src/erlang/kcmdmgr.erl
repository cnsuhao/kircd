%% vim: set fileencoding=utf-8 :
-module(kcmdmgr).
-export([exec_cmd/3]).

-include("kerrcode.hrl").

%% PASS <password>
exec_cmd("PASS", User, Args) ->
    if
        length(Args) == 0 ->
            kuser:send_packet(User, ?ERR_NEEDMOREPARAMS);
        true ->
            kuser:change_password(User, lists:nth(1, Args)),
            io:format("good!~n")
    end;

%% 
exec_cmd("USER", User, Args) ->
    io:format("USER~n");

exec_cmd(_, User, Args) ->
    io:format("other~n").

