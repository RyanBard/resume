-module(json_util).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume.hrl").

-export([
  rfc4627_to_mongodb/1,
  rfc4627_to_emongo/1,
  mongodb_to_rfc4627/1,
  string_to_mongodb/1,
  string_to_emongo/1,
  string_to_rfc4627/1,
  mongodb_to_string/1,
  emongo_to_string/1,
  rfc4627_to_string/1
]).


string_to_mongodb(Json) ->
  J = string_to_rfc4627(Json),
  rfc4627_to_mongodb(J).


string_to_emongo(Json) ->
  J = string_to_rfc4627(Json),
  rfc4627_to_emongo(J).


string_to_rfc4627(Json) ->
  {ok, Result, []} = rfc4627:decode(Json),
  Result.


mongodb_to_string(Json) ->
  J = mongodb_to_rfc4627(Json),
  rfc4627_to_string(J).


emongo_to_string(Json) ->
  J = emongo_to_rfc4627(Json),
  rfc4627_to_string(J).


rfc4627_to_string(Json) ->
  list_to_binary(rfc4627:encode(Json)).


rfc4627_to_mongodb(Boolean) when is_boolean(Boolean) -> Boolean;
rfc4627_to_mongodb(Number) when is_number(Number) -> Number;
rfc4627_to_mongodb(Binary) when is_binary(Binary) -> Binary;
rfc4627_to_mongodb(null) -> null;
rfc4627_to_mongodb([]) -> [];
rfc4627_to_mongodb([H | T]) -> [rfc4627_to_mongodb(H) | rfc4627_to_mongodb(T)];
rfc4627_to_mongodb({obj, FieldKeyVals}) ->
  lists:foldl(fun(X, Acc) ->
    erlang:append_element(erlang:append_element(Acc, list_to_atom(element(1, X))), rfc4627_to_mongodb(element(2, X)))
  end, {}, FieldKeyVals).


rfc4627_to_emongo(Boolean) when is_boolean(Boolean) -> Boolean;
rfc4627_to_emongo(Number) when is_number(Number) -> Number;
rfc4627_to_emongo(Binary) when is_binary(Binary) -> Binary;
rfc4627_to_emongo(null) -> undefined;
rfc4627_to_emongo([]) -> {array, []};
rfc4627_to_emongo([H | T]) ->
  {array, Rest} = rfc4627_to_emongo(T),
  {array, [rfc4627_to_emongo(H) | Rest]};
rfc4627_to_emongo({obj, []}) -> [];
rfc4627_to_emongo({obj, [H | T]}) ->
  Rest = rfc4627_to_emongo({obj, T}),
  [rfc4627_to_emongo(H) | Rest];
rfc4627_to_emongo({K, V}) ->
  {list_to_binary(K), rfc4627_to_emongo(V)}.


emongo_to_rfc4627(Boolean) when is_boolean(Boolean) -> Boolean;
emongo_to_rfc4627(Number) when is_number(Number) -> Number;
emongo_to_rfc4627(Binary) when is_binary(Binary) -> Binary;
emongo_to_rfc4627(null) -> null;
emongo_to_rfc4627(undefined) -> null;
emongo_to_rfc4627({array, []}) -> [];
emongo_to_rfc4627({array, [H | T]}) ->
  Rest = emongo_to_rfc4627({array, T}),
  [emongo_to_rfc4627(H) | Rest];
emongo_to_rfc4627([]) -> {obj, []};
emongo_to_rfc4627([H | T]) ->
  {obj, Rest} = emongo_to_rfc4627(T),
  {obj, [emongo_to_rfc4627(H) | Rest]};
emongo_to_rfc4627({struct, []}) -> {obj, []};
emongo_to_rfc4627({struct, [H | T]}) ->
  {obj, Rest} = emongo_to_rfc4627({struct, T}),
  {obj, [emongo_to_rfc4627(H) | Rest]};
emongo_to_rfc4627({K, V}) ->
  {binary_to_list(K), emongo_to_rfc4627(V)}.


mongodb_to_rfc4627(Boolean) when is_boolean(Boolean) -> Boolean;
mongodb_to_rfc4627(Number) when is_number(Number) -> Number;
mongodb_to_rfc4627(Binary) when is_binary(Binary) -> Binary;
mongodb_to_rfc4627(null) -> null;
mongodb_to_rfc4627([]) -> [];
mongodb_to_rfc4627([H | T]) -> [mongodb_to_rfc4627(H) | mongodb_to_rfc4627(T)];
mongodb_to_rfc4627(Tuple) when is_tuple(Tuple) andalso ((tuple_size(Tuple) rem 2) =:= 0) ->
  FlattenedKeyVals = tuple_to_list(Tuple),
  {Keys, Vals} = split_key_vals(FlattenedKeyVals, {[], []}),
  KeyVals = lists:zip(Keys, Vals),
  {obj, [{atom_to_list(K), mongodb_to_rfc4627(V)} || {K, V} <- KeyVals]}.


split_key_vals([], {KeysAcc, ValsAcc}) ->
  {lists:reverse(KeysAcc), lists:reverse(ValsAcc)};

split_key_vals([K, V | T], {KeysAcc, ValsAcc}) ->
  split_key_vals(T, {[K | KeysAcc], [V | ValsAcc]}).

% mongodb_to_rfc4627({obj, FieldKeyVals}) ->
%   lists:foldl(fun(X, Acc) ->
%     erlang:append_element(erlang:append_element(Acc, list_to_atom(element(1, X))), mongodb_to_rfc4627(element(2, X)))
%   end, {}, FieldKeyVals).
