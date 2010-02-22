%% vim: set fileencoding=utf-8 :
-module(kuser).
-export([send_packet/2, change_password/2]).

-include("kuser.hrl").

change_password(User, NewPassword) ->
    NewUser  = User#user_record{pass=NewPassword},
    kusermgr:update_user(NewUser).

send_packet(User, Msg) ->
    Socket = User#user_record.socket,
    gen_tcp:send(Socket, Msg ++ "\r\n").

