-module(resume_dao).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume.hrl").

-export([create/1, delete/1, update/2, find/1, find_all/0, find_by_user_id/1]).


create(Resume) ->
  emongo:insert(?POOL, ?RESUME_COLL, Resume).

delete(ResumeId) ->
  emongo:delete(?POOL, ?RESUME_COLL, [{<<"resume_id">>, ResumeId}]).

update(ResumeId, Resume) ->
  emongo:update(?POOL, ?RESUME_COLL, [{<<"resume_id">>, ResumeId}], Resume).

find(ResumeId) ->
  case emongo:find_one(?POOL, ?RESUME_COLL, [{<<"resume_id">>, ResumeId}]) of
    [Resume] -> Resume;
           _ -> undefined
  end.

find_all() ->
  emongo:find_all(?POOL, ?RESUME_COLL, []).

find_by_user_id(UserId) ->
  emongo:find_all(?POOL, ?RESUME_COLL, [{<<"user_id">>, UserId}]).
