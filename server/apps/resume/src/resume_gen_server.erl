-module(resume_gen_server).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume.hrl").

-export([
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  init/1,
  terminate/2
]).

-export([
  sync_stop/0,
  async_stop/0,
  no_reply_ex/0,
  sync_hello/0,
  async_hello/0
]).

-behaviour(gen_server).


sync_stop() ->
  gen_server:call(?MODULE, stop_ex).

async_stop() ->
  gen_server:cast(?MODULE, stop_ex).

no_reply_ex() ->
  gen_server:call(?MODULE, no_reply_ex).

sync_hello() ->
  gen_server:call(?MODULE, <<"hello">>).

async_hello() ->
  gen_server:cast(?MODULE, <<"hello">>).



code_change({down, _OldVsn}, State, _Extra) ->
  {ok, State};

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



handle_call(stop_ex, From, State) ->
  ?DEBUG("In handle_call(~p, ~p, ~p)~n", [stop_ex, From, State]),
  Reply = stopping,
  {stop, normal, Reply, State};

handle_call(no_reply_ex, From, State) ->
  ?DEBUG("In handle_call(~p, ~p, ~p)~n", [no_reply_ex, From, State]),
  {noreply, State};

handle_call(Request, From, State) ->
  ?DEBUG("In handle_call(~p, ~p, ~p)~n", [Request, From, State]),
  Reply = Request,
  {reply, Reply, State}.



handle_cast(stop_ex, State) ->
  ?DEBUG("In handle_cast(~p, ~p)~n", [stop_ex, State]),
  {stop, normal, State};

handle_cast(Request, State) ->
  ?DEBUG("In handle_cast(~p, ~p)~n", [Request, State]),
  {noreply, State}.



handle_info(_Info, State) ->
  {noreply, State}.



init(_Args) ->
  InitialState = [],
  {ok, InitialState}.



terminate(Reason, State) ->
  ?DEBUG("In terminate(~p, ~p)~n", [Reason, State]),
  ok.
