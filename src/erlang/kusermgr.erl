%% vim: set fileencoding=utf-8 :
%% Socket ==> User
-module(kusermgr).
-export([init/0, get_user/2, remove_user/2]).

-include("kuser.hrl").

init() ->
    Tbl = ets:new(?MODULE, [set]),
    {ok, Tbl}.

get_user(Socket, Tbl) ->
    case ets:lookup(Tbl, Socket) of
        [] ->
            User = #kuser{socket=Socket, nickname=none, pass=none, status=?KUSER_ST_PASS},
            ets:insert(Tbl, User),
            {ok, User};
        [User] -> {ok, User}
    end.

remove_user(Socket, Tbl) ->
    ets:delete(Tbl, Socket).
