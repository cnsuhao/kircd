%% vim: set fileencoding=utf-8 :
-module(kuser).
-export([send_errcode/2, change_password/2,]).

-include("kuser.hrl").

send_errcode(User, ErrCode) ->
    io:format("errcode ~p~n", [User]),
    Socket = User#user_record.socket,
    gen_tcp:send(Socket, ErrCode).

change_password(User, NewPassword) ->
    Socket   = User#user_record.socket,
    NickName = User#user_record.nickname,
    Status   = User#user_record.status,

    NewUser  = #user_record{socket=Socket, nickname=NickName, pass=NewPassword, status=Status},
    kusermgr:update(NewUser).

