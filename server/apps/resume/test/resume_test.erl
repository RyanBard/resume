-module(resume_test).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume_test.hrl").

-compile(export_all).

-define(TIMEOUT, 1000).


run_test_() ->
  [
    {inorder,
      {setup,
        fun setup/0,
        fun teardown/1,
        [
          fun test_create/0,
          fun test_create_error/0,
          fun test_find/0,
          fun test_find_not_found/0,
          fun test_update/0,
          fun test_update_resume_id_not_specified/0,
          fun test_update_error/0,
          fun test_update_not_found/0,
          fun test_delete/0,
          fun test_delete_resume_id_not_specified/0,
          fun test_delete_error/0,
          fun test_delete_not_found/0,
          fun test_find_all/0,
          fun test_find_by_user_id/0
        ]
      }
    }
  ].


setup() ->
  rh:start(),
  % ok = application:start(resume),
  meck:new(resume_service),
  meck:expect(resume_service, create, fun
    ({obj, [{"name", <<"Smith">>}]}) -> ok;
    ({obj, [{"name", <<"Ryan">>}]}) -> {error, ?CREATE_ERR_MSG}
  end),
  meck:expect(resume_service, update, fun
    (?RESUME_ID_1, {obj, [{"name", <<"JOHN">>}]}) -> ok;
    (?RESUME_ID_1, {obj, [{"name", <<"Ryan">>}]}) -> {error, ?UPDATE_ERR_MSG};
    (?WILL_NOT_FIND, _) -> {error, not_found}
  end),
  meck:expect(resume_service, delete, fun
    (?RESUME_ID_1) -> ok;
    (?RESUME_ID_3) -> {error, ?DELETE_ERR_MSG};
    (?WILL_NOT_FIND) -> {error, not_found}
  end),
  meck:expect(resume_service, find, fun
    (?RESUME_ID_1) -> resume1();
    (?WILL_NOT_FIND) -> undefined
  end),
  meck:expect(resume_service, find_all, fun() -> [resume1(), resume2(), resume3()] end),
  meck:expect(resume_service, find_by_user_id, fun(_UserId) -> [resume1(), resume2()] end),
  ok.

teardown(_Args) ->
  meck:unload(resume_service),
  ok = application:stop(resume),
  ok.


test_create() ->
  {ok, Result} = httpc:request(post, http_request(<<"/resumes">>, <<"{\"name\" : \"Smith\"}">>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 201, "Created"}, StatusLine),
  ?assertEqual("", Body),
  ok.


test_create_error() ->
  {ok, Result} = httpc:request(post, http_request(<<"/resumes">>, <<"{\"name\" : \"Ryan\"}">>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 422, "Unprocessable Entity"}, StatusLine),
  ?assertEqual(?CREATE_ERR_MSG, Body),
  ok.


test_find() ->
  ResumeId = ?RESUME_ID_1,
  {ok, Result} = httpc:request(get, http_request(<<"/resumes/", ResumeId/binary>>), http_options(), []),
  {StatusLine, Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 200, "OK"}, StatusLine),
  ?assertEqual({"content-type", "application/json"}, lists:keyfind("content-type", 1, Headers)),
  Expected = lists:flatten(io_lib:format("{\"name\":\"~s\",\"user_id\":\"~s\",\"resume_id\":\"~s\"}",
                                         [name1(), user_id1(), ?RESUME_ID_1])),
  ?assertEqual(Expected, Body),
  ok.


test_find_not_found() ->
  ResumeId = ?WILL_NOT_FIND,
  {ok, Result} = httpc:request(get, http_request(<<"/resumes/", ResumeId/binary>>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 404, "Not Found"}, StatusLine),
  ?assertEqual("", Body),
  ok.


test_update() ->
  ResumeId = ?RESUME_ID_1,
  {ok, Result} = httpc:request(put, http_request(<<"/resumes/", ResumeId/binary>>, <<"{\"name\" : \"JOHN\"}">>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 204, "No Content"}, StatusLine),
  ?assertEqual("", Body),
  ok.


test_update_resume_id_not_specified() ->
  {ok, Result} = httpc:request(put, http_request(<<"/resumes">>, <<"{\"name\" : \"JOHN\"}">>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 400, "Bad Request"}, StatusLine),
  ?assertEqual("", Body),
  ok.


test_update_error() ->
  ResumeId = ?RESUME_ID_1,
  {ok, Result} = httpc:request(put, http_request(<<"/resumes/", ResumeId/binary>>, <<"{\"name\" : \"Ryan\"}">>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 422, "Unprocessable Entity"}, StatusLine),
  ?assertEqual(?UPDATE_ERR_MSG, Body),
  ok.


test_update_not_found() ->
  ResumeId = ?WILL_NOT_FIND,
  {ok, Result} = httpc:request(put, http_request(<<"/resumes/", ResumeId/binary>>, <<"{\"name\" : \"JOHN\"}">>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 404, "Not Found"}, StatusLine),
  ?assertEqual("", Body),
  ok.


test_delete() ->
  ResumeId = ?RESUME_ID_1,
  {ok, Result} = httpc:request(delete, http_request(<<"/resumes/", ResumeId/binary>>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 204, "No Content"}, StatusLine),
  ?assertEqual("", Body),
  ok.


test_delete_resume_id_not_specified() ->
  {ok, Result} = httpc:request(delete, http_request(<<"/resumes">>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 400, "Bad Request"}, StatusLine),
  ?assertEqual("", Body),
  ok.


test_delete_error() ->
  ResumeId = ?RESUME_ID_3,
  {ok, Result} = httpc:request(delete, http_request(<<"/resumes/", ResumeId/binary>>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 422, "Unprocessable Entity"}, StatusLine),
  ?assertEqual(?DELETE_ERR_MSG, Body),
  ok.


test_delete_not_found() ->
  ResumeId = ?WILL_NOT_FIND,
  {ok, Result} = httpc:request(delete, http_request(<<"/resumes/", ResumeId/binary>>), http_options(), []),
  {StatusLine, _Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 404, "Not Found"}, StatusLine),
  ?assertEqual("", Body),
  ok.


test_find_all() ->
  {ok, Result} = httpc:request(get, http_request(<<"/resumes">>), http_options(), []),
  {StatusLine, Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 200, "OK"}, StatusLine),
  ?assertEqual({"content-type", "application/json"}, lists:keyfind("content-type", 1, Headers)),
  Expected = lists:flatten(io_lib:format("[{\"name\":\"~s\",\"user_id\":\"~s\",\"resume_id\":\"~s\"}," ++
                                          "{\"name\":\"~s\",\"user_id\":\"~s\",\"resume_id\":\"~s\"}," ++
                                          "{\"name\":\"~s\",\"user_id\":\"~s\",\"resume_id\":\"~s\"}]",
                                         [name1(), user_id1(), ?RESUME_ID_1,
                                          name1(), user_id1(), resume_id2(),
                                          name2(), user_id2(), ?RESUME_ID_3])),
  ?assertEqual(Expected, Body),
  ok.


test_find_by_user_id() ->
  UserId = user_id1(),
  {ok, Result} = httpc:request(get, http_request(<<"/resumes?user_id=", UserId/binary>>), http_options(), []),
  {StatusLine, Headers, Body} = Result,
  ?assertEqual({"HTTP/1.1", 200, "OK"}, StatusLine),
  ?assertEqual({"content-type", "application/json"}, lists:keyfind("content-type", 1, Headers)),
  Expected = lists:flatten(io_lib:format("[{\"name\":\"~s\",\"user_id\":\"~s\",\"resume_id\":\"~s\"}," ++
                                          "{\"name\":\"~s\",\"user_id\":\"~s\",\"resume_id\":\"~s\"}]",
                                         [name1(), user_id1(), ?RESUME_ID_1,
                                          name1(), user_id1(), resume_id2()])),
  ?assertEqual(Expected, Body),
  ok.


http_request(Path) ->
  {url(Path), []}.

http_request(Path, MsgBody) ->
  {url(Path), [], "application/json", MsgBody}.


url(Path) ->
  "http://localhost:8080" ++ to_list(Path).


http_options() ->
  [{timeout, ?TIMEOUT}, {connect_timeout, ?TIMEOUT}].


to_list(List) when is_list(List) ->
  List;

to_list(Binary) when is_binary(Binary) ->
  binary_to_list(Binary).

resume1() ->
  {obj, [
    {"name", name1()},
    {"user_id", user_id1()},
    {"resume_id", ?RESUME_ID_1}
  ]}.

resume2() ->
  {obj, [
    {"name", name1()},
    {"user_id", user_id1()},
    {"resume_id", resume_id2()}
  ]}.

resume3() ->
  {obj, [
    {"name", name2()},
    {"user_id", user_id2()},
    {"resume_id", ?RESUME_ID_3}
  ]}.

resume_id2() ->
  <<"1002">>.

name1() ->
  <<"John">>.

name2() ->
  <<"Ryan">>.

user_id1() ->
  <<"1234">>.

user_id2() ->
  <<"9999">>.
