-module(resume_dao_test).

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
  emongo:delete_sync(?POOL, ?RESUME_COLL, []),
  ok.

teardown(_) ->
  emongo:delete_sync(?POOL, ?RESUME_COLL, []),
  ok = application:stop(resume),
  ok.

test_create() ->
  ?assertEqual([], resume_dao:find_all()),
  Result = resume_dao:create(test_util:emongo_resume(<<"John Bard">>)),
  ?assertEqual(ok, Result),
  FindResult = resume_dao:find(?RESUME_ID),
  ?assertMatch([
    {<<"_id">>, _},
    {<<"name">>, <<"John Bard">>},
    {<<"resume_id">>, ?RESUME_ID},
    {<<"user_id">>, ?USER_ID}
  ], FindResult),
  ok.

test_find_all() ->
  Result = resume_dao:find_all(),
  ?assertMatch([
    [
      {<<"_id">>, _},
      {<<"name">>, <<"John Bard">>},
      {<<"resume_id">>, ?RESUME_ID},
      {<<"user_id">>, ?USER_ID}
    ]
  ], Result),
  ok.

test_update() ->
  Result = resume_dao:update(?RESUME_ID, test_util:emongo_resume(<<"John Ryan Bard">>)),
  ?assertEqual(ok, Result),
  FindResult = resume_dao:find(?RESUME_ID),
  ?assertMatch([
    {<<"_id">>, _},
    {<<"name">>, <<"John Ryan Bard">>},
    {<<"resume_id">>, ?RESUME_ID},
    {<<"user_id">>, ?USER_ID}
  ], FindResult),
  ok.

test_delete() ->
  FindAll1Result = resume_dao:find_all(),
  ?assertMatch([
    [
      {<<"_id">>, _},
      {<<"name">>, <<"John Ryan Bard">>},
      {<<"resume_id">>, ?RESUME_ID},
      {<<"user_id">>, ?USER_ID}
    ]
  ], FindAll1Result),
  Result = resume_dao:delete(?RESUME_ID),
  ?assertEqual(ok, Result),
  ?assertEqual([], resume_dao:find_all()),
  ok.

test_find_by_user_id() ->
  ?assertEqual(ok, resume_dao:create(test_util:emongo_resume(<<"John Ryan Bard">>, ?USER_ID))),
  ?assertEqual(ok, resume_dao:create(test_util:emongo_resume(<<"Ryan Bard">>, ?USER_ID_2))),
  ?assertEqual(2, length(resume_dao:find_all())),
  Result = resume_dao:find_by_user_id(?USER_ID),
  ?assertMatch([
    [
      {<<"_id">>, _},
      {<<"name">>, <<"John Ryan Bard">>},
      {<<"resume_id">>, ?RESUME_ID},
      {<<"user_id">>, ?USER_ID}
    ]
  ], Result),
  ok.
