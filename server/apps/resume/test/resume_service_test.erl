-module(resume_service_test).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume_test.hrl").

-compile(export_all).


run_test_() ->
  [
    {inorder, {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun test_create/0,
        fun test_find_all/0,
        fun test_update/0,
        fun test_delete/0,
        fun test_find_by_user_id/0
      ]
    }}
  ].

setup() ->
  rh:start(),
  % ok = application:start(resume),
  meck:new(resume_dao),
  meck:expect(resume_dao, create, fun
    ([{"name", <<"John Bard">>}]) -> ok;
    ([{"name", <<"Ryan">>}]) -> {error, ?CREATE_ERR_MSG}
  end),
  meck:expect(resume_dao, update, fun
    (?RESUME_ID_1, [{"name", <<"JOHN">>}]) -> ok;
    (?RESUME_ID_1, [{"name", <<"Ryan">>}]) -> {error, ?UPDATE_ERR_MSG};
    (?WILL_NOT_FIND, _) -> {error, not_found}
  end),
  meck:expect(resume_dao, delete, fun
    (?RESUME_ID_1) -> ok;
    (?RESUME_ID_3) -> {error, ?DELETE_ERR_MSG};
    (?WILL_NOT_FIND) -> {error, not_found}
  end),
  meck:expect(resume_dao, find, fun
    (?RESUME_ID_1) -> resume1();
    (?WILL_NOT_FIND) -> undefined
  end),
  %TODO - push these out to common test util? (these are defined in resume_test.erl right not, haven't ported these over yet)
  meck:expect(resume_dao, find_all, fun() -> [resume1(), resume2(), resume3()] end),
  meck:expect(resume_dao, find_by_user_id, fun(_UserId) -> [resume1(), resume2()] end),
  ok.

teardown(_Args) ->
  meck:unload(resume_service),
  ok = application:stop(resume),
  ok.

test_create() ->
  Result = resume_service:create(test_util:rfc4627_resume(<<"John Bard">>)),
  ?assertEqual(ok, Result),
  ok.

test_delete() ->
  Result = resume_service:delete(?RESUME_ID),
  ?assertEqual(ok, Result),
  ok.

test_update() ->
  Result = resume_service:update(?RESUME_ID, test_util:rfc4627_resume(<<"John Ryan Bard">>)),
  ?assertEqual(ok, Result),
  ok.

test_find() ->
  Result = resume_service:find(?RESUME_ID),
  ?assertEqual(ok, Result),
  ok.

test_find_all() ->
  Result = resume_service:find_all(),
  ?assertEqual(ok, Result),
  ok.

test_find_by_user_id() ->
  Result = resume_service:find_by_user_id(?USER_ID),
  ?assertEqual(ok, Result),
  ok.

%%
%%
%%

resume1() ->
  [
    {<<"name">>,      ?NAME_1},
    {<<"resume_id">>, ?RESUME_ID_1},
    {<<"user_id">>,   ?USER_ID_1}
  ].

resume2() ->
  [
    {<<"name">>,      ?NAME_2},
    {<<"resume_id">>, ?RESUME_ID_2},
    {<<"user_id">>,   ?USER_ID_2}
  ].

resume3() ->
  [
    {<<"name">>,      ?NAME_3},
    {<<"resume_id">>, ?RESUME_ID_3},
    {<<"user_id">>,   ?USER_ID_3}
  ].
