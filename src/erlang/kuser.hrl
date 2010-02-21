-define(KUSER_ST_PASS, kuser_st_pass).
-define(KUSER_ST_REGISTERED, kuser_st_registered).

-record(user_record, {socket, nickname, pass, status}).
