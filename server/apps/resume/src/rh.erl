-module(rh).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume.hrl").

-compile(export_all).
% -export([start/0]).


list_reg() ->
  lists:foreach(fun(X) -> io:format("~p~n", [X]) end, registered()).

start() ->
  start_deps(),
  ensure_started(resume),
  ok.

start_deps() ->
  lists:foreach(fun(Dep) -> ensure_started(Dep) end, deps()),
  ok.

ensure_started(AppName) ->
  case application:start(AppName) of
    ok ->
      ok;
    {error, {already_started, AppName}} ->
      ok
  end.

deps() ->
  [sasl, inets, emongo, ranch, cowlib, cowboy].
