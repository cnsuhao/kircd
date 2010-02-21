%% vim: set fileencoding=utf-8 :
-module(kuser).
-export([send_errcode/2]).

-include("kuser.hrl").

send_errcode(User, ErrCode) ->
    io:format("errcode ~p~n", [User]),
    Socket = User#user_record.socket,
    gen_tcp:send(Socket, ErrCode).

