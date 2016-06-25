-module(resume_app).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume.hrl").

-export([start/2, stop/1]).

-behaviour(application).


start(_Type, Args) ->
  ?DEBUG("resume_app startup args are: ~p~n", [Args]),
  {mongodb_host, MongoHost} = lists:keyfind(mongodb_host, 1, Args),
  {mongodb_port, MongoPort} = lists:keyfind(mongodb_port, 1, Args),
  {mongodb_num_connections, MongoNumConnections} = lists:keyfind(mongodb_num_connections, 1, Args),
  connect_to_mongo(MongoHost, MongoPort, MongoNumConnections),
  {concurrent_reqs_per_socket, ConcReqPerSocket} = lists:keyfind(concurrent_reqs_per_socket, 1, Args),
  {port, Port} = lists:keyfind(port, 1, Args),
  ListenerRef = resume_listener,
  State = [
    {cowboy_listeners, [ListenerRef]}
  ],
  {ok, _} = start_cowboy(ListenerRef, ConcReqPerSocket, Port),
  {ok, Pid} = supervisor:start_link({local, resume_sup}, resume_sup, []),
  {ok, Pid, State}.

start_cowboy(ListenerRef, ConcReqPerSocket, Port) ->
  Dispatch = cowboy_router:compile([
    %% {HostMatch, list({PathMatch, Handler, Opts})}
    {'_', [
      {"/resumes/[:resume_id]", resume_http_handler, []}
    ]}
  ]),
  cowboy:start_http(ListenerRef, ConcReqPerSocket, [{port, Port}], [
    {env, [{dispatch, Dispatch}]},
    {max_keepalive, 5}
  ]).

  connect_to_mongo(Host, Port, NumConnections) ->
    case Port of
      undefined -> emongo:add_pool(?POOL, Host, 27017, ?RESUMEDB, NumConnections);
      _ -> emongo:add_pool(?POOL, Host, Port, ?RESUMEDB, NumConnections)
    end.

stop(State) ->
  % exit(whereis(resume_sup), kill),
  % TODO - work try-catch into these so an error in one won't skip the others
  {cowboy_listeners, Listeners} = lists:keyfind(cowboy_listeners, 1, State),
  lists:foreach(fun(L) -> ok = cowboy:stop_listener(L) end, Listeners),
  emongo:remove_pool(pool1),
  ok.
