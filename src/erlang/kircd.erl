%% vim: set fileencoding=utf-8 :
-module(kircd).
-export([start/0]).

start() ->
    {ok,Listen} = gen_tcp:listen(7788, [list, {packet,line}, {reuseaddr,true}]),
    spawn(fun() -> listen_conn(Listen) end).

listen_conn(Listen) ->
    {ok, UserMgrState} = kusermgr:init(),
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            spawn(fun() -> listen_conn(Listen) end),
            conn_loop(UserMgrState);
        {error, Reason} ->
            io:format("listenfd ~w closed~n", [Listen])
    end.

conn_loop(UserMgrState) ->
    receive
        {tcp, Socket, DataList} ->
            io:format("~w recv ~p~n", [Socket, DataList]),
            DataList1 = string:strip(DataList, right, $\n),
            DataList2 = string:strip(DataList1, right, $\r),
            L = string:len(DataList2),
            if
                L > 0 ->
                    [Cmd|Args] = string:tokens(DataList2, " "),
                    {ok, User} = kusermgr:get_user(Socket, UserMgrState),
                    kcmdmgr:exec_cmd(Cmd, User, Args);
                true ->
                    do_nothing
            end,
            conn_loop(UserMgrState);
        {tcp_closed, Socket} ->
            io:format("~w closed~n", [Socket]),
            kusermgr:remove_user(Socket, UserMgrState),
            ok
    end.

