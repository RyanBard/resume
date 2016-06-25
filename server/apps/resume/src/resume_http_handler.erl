-module(resume_http_handler).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume.hrl").

-export([init/3, handle/2, terminate/3]).

-behaviour(cowboy_http_handler).

-define(HEADERS, [{<<"content-type">>, <<"application/json">>}]).


init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.


handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {ok, Resp} = handle_request(Method, Req2),
  % StatusCode = 200,
  % Headers = [{<<"content-type">>, <<"text/plain">>}],
  % Message = <<"OK">>,
  % {ok, Resp} = cowboy_req:reply(StatusCode, Headers, Message, Req),
  {ok, Resp, State}.

handle_request(<<"POST">>, Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req), % TODO - use a different max body length than the default?
  case resume_service:create(json_util:string_to_rfc4627(Body)) of
    ok ->
      ResumeId = <<"123">>,
      cowboy_req:reply(201, [{<<"Location">>, ResumeId}], <<>>, Req2);
    {error, Msg} ->
      cowboy_req:reply(422, [], Msg, Req2)
  end;

handle_request(<<"GET">>, Req) ->
  {ResumeId, Req2} = cowboy_req:binding(resume_id, Req),
  case ResumeId of
    undefined ->
      {UserId, Req3} = cowboy_req:qs_val(<<"user_id">>, Req2),
      case UserId of
        undefined ->
          Results = resume_service:find_all(),
          cowboy_req:reply(200, ?HEADERS, json_util:rfc4627_to_string(Results), Req3);
        _ ->
          case resume_service:find_by_user_id(UserId) of
            Res ->
              cowboy_req:reply(200, ?HEADERS, json_util:rfc4627_to_string(Res), Req3)
          end
      end;
    _ ->
      case resume_service:find(ResumeId) of
        undefined ->
          cowboy_req:reply(404, [], <<>>, Req2);
        Res ->
          cowboy_req:reply(200, ?HEADERS, json_util:rfc4627_to_string(Res), Req2)
      end
  end;

handle_request(<<"PUT">>, Req) ->
  {ResumeId, Req2} = cowboy_req:binding(resume_id, Req),
  case ResumeId of
    undefined ->
      cowboy_req:reply(400, [], <<>>, Req2);
    _ ->
      {ok, Body, Req3} = cowboy_req:body(Req2), % TODO - use a different max body length than the default?
      % TODO - distinguish the difference between 404 and 422?
      case resume_service:update(ResumeId, json_util:string_to_rfc4627(Body)) of
        ok ->
          cowboy_req:reply(204, [], <<>>, Req3);
        {error, not_found} ->
          cowboy_req:reply(404, [], <<>>, Req3);
        {error, Msg} ->
          cowboy_req:reply(422, [], Msg, Req3)
      end
  end;

handle_request(<<"DELETE">>, Req) ->
  {ResumeId, Req2} = cowboy_req:binding(resume_id, Req),
  case ResumeId of
    undefined ->
      cowboy_req:reply(400, [], <<>>, Req2);
    _ ->
      case resume_service:delete(ResumeId) of
        ok ->
          cowboy_req:reply(204, [], <<>>, Req2);
        {error, not_found} ->
          cowboy_req:reply(404, [], <<>>, Req2);
        {error, Msg} ->
          cowboy_req:reply(422, [], Msg, Req2)
      end
  end.


terminate(_Reason, _Req, _State) ->
  ok.
