%% vim: set fileencoding=utf-8 :
%% Socket ==> User
-module(kusermgr).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, get_user/1, update_user/1, remove_user/1]).

-include("kuser.hrl").

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
get_user(Socket) -> gen_server:call(?MODULE, {get_user, Socket}).
update_user(User) -> gen_server:call(?MODULE, {update_user, User}).
remove_user(Socket) -> gen_server:call(?MODULE, {remove_user, Socket}).

init([]) ->
    Tbl = ets:new(?MODULE, []),
    {ok, Tbl}.

handle_call({get_user, Socket}, _From, Tbl) ->
    Response = case ets:lookup(Tbl, Socket) of
        [] ->
            User = #user_record{socket=Socket, nickname=none, pass=none, status=?KUSER_ST_PASS},
            ets:insert(Tbl, User),
            {ok, User};
        [User] ->
            {ok, User}
    end,
    {reply, Response, Tbl};

handle_call({update_user, User}, _From, Tbl) ->
    ets:insert(Tbl, User),
    {reply, ok, Tbl};

handle_call({remove_user, Socket}, _From, Tbl) ->
    ets:delete(Tbl, Socket),
    {reply, ok, Tbl}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

