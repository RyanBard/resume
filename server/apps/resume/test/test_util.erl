-module(test_util).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume_test.hrl").

-compile(export_all).


rfc4627_resume(Name) ->
  rfc4627_resume(Name, ?USER_ID).

rfc4627_resume(Name, UserId) ->
  json_util:mongodb_to_rfc4627(mongodb_resume(Name, UserId)).


mongodb_resume(Name) ->
  {name, Name, resume_id, ?RESUME_ID, user_id, ?USER_ID}.

mongodb_resume(Name, UserId) ->
  {name, Name, resume_id, ?RESUME_ID, user_id, UserId}.


emongo_resume(Name) ->
  emongo_resume(Name, ?USER_ID).

emongo_resume(Name, UserId) ->
  json_util:rfc4627_to_emongo(json_util:mongodb_to_rfc4627(mongodb_resume(Name, UserId))).
