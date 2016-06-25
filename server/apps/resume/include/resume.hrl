-include_lib("eunit/include/eunit.hrl").

-define(POOL, pool1).

-define(RESUMEDB, "RESUMEDB").
-define(RESUME_COLL, "resumes").

-define(DEBUG, fun(DbgFormat, DbgArgs) -> ?debugFmt(DbgFormat, DbgArgs) end).
