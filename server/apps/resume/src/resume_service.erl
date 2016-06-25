-module(resume_service).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume.hrl").

-export([create/1, delete/1, update/2, find/1, find_all/0, find_by_user_id/1]).


create(Resume) ->
  resume_dao:create(json_util:rfc4627_to_emongo(Resume)).

delete(ResumeId) ->
  resume_dao:delete(ResumeId).

update(ResumeId, Resume) ->
  resume_dao:update(ResumeId, json_util:rfc4627_to_emongo(Resume)).

find(ResumeId) ->
  json_util:emongo_to_rfc4627(resume_dao:find(ResumeId)).

find_all() ->
  json_util:emongo_to_rfc4627(resume_dao:find_all()).

find_by_user_id(UserId) ->
  json_util:emongo_to_rfc4627(resume_dao:find_by_user_id(UserId)).
