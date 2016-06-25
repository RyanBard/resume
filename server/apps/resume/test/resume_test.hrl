-include_lib("eunit/include/eunit.hrl").
-include("resume.hrl").

-define(RESUME_ID, <<"1001">>).

-define(RESUME_ID_1, ?RESUME_ID).
-define(RESUME_ID_2, <<"1002">>).
-define(RESUME_ID_3, <<"2001">>).

-define(WILL_NOT_FIND, <<"will_not_find">>).

-define(USER_ID, <<"1234">>).

-define(USER_ID_1, ?USER_ID).
-define(USER_ID_2, <<"2345">>).
-define(USER_ID_3, <<"3456">>).

-define(NAME, <<"John Ryan Bard">>).

-define(NAME_1, ?NAME).
-define(NAME_2, <<"Ryan Bard">>).
-define(NAME_3, <<"Jane Doe">>).

% TODO - These should probably be 409 Conflicts rather than 422 Unprocessable
-define(CREATE_ERR_MSG, "Mock Create Error: name already taken").
-define(UPDATE_ERR_MSG, "Mock Update Error: name already taken").
-define(DELETE_ERR_MSG, "Mock Delete Error: resume in use").
