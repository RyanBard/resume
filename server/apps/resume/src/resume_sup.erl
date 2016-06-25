-module(resume_sup).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume.hrl").

-export([init/1]).

-behaviour(supervisor).


init(_Args) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1,
  MaxPerTime = 100,
  % ShutdownTimeout = 1000,
  % Modules = [resume_gen_server, resume_service, resume_dao],
  % GenServerMFA = {gen_server, start_link, [{local, resume_gen_server}, resume_gen_server, [], []]},
  % ChildSpec = {resume, GenServerMFA, permanent, ShutdownTimeout, worker, Modules},
  % ChildSpecs = [ChildSpec],
  ChildSpecs = [],
  {ok, {{RestartStrategy, MaxRestarts, MaxPerTime}, ChildSpecs}}.
