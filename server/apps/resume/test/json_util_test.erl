-module(json_util_test).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume_test.hrl").

-compile(export_all).


string_to_emongo_test() ->
  ?assertEqual([], json_util:string_to_emongo(<<"{}">>)),
  ?assertEqual({array, []}, json_util:string_to_emongo(<<"[]">>)),
  ?assertEqual({array, [[]]}, json_util:string_to_emongo(<<"[{}]">>)),
  ?assertEqual({array, [[{<<"name">>, <<"string_val">>}]]}, json_util:string_to_emongo(<<"[{\"name\" : \"string_val\"}]">>)),
  ?assertEqual({array, [[{<<"name">>, <<"">>}]]}, json_util:string_to_emongo(<<"[{\"name\" : \"\"}]">>)),
  ?assertEqual({array, [[{<<"name">>, 123}]]}, json_util:string_to_emongo(<<"[{\"name\" : 123}]">>)),
  ?assertEqual({array, [[{<<"name">>, 1.1}]]}, json_util:string_to_emongo(<<"[{\"name\" : 1.1}]">>)),
  ?assertEqual({array, [[{<<"name">>, true}]]}, json_util:string_to_emongo(<<"[{\"name\" : true}]">>)),
  ?assertEqual({array, [[{<<"name">>, false}]]}, json_util:string_to_emongo(<<"[{\"name\" : false}]">>)),
  ?assertEqual({array, [[{<<"name">>, undefined}]]}, json_util:string_to_emongo(<<"[{\"name\" : null}]">>)),
  ?assertEqual({array, [[{<<"name">>, {array, []}}]]}, json_util:string_to_emongo(<<"[{\"name\" : []}]">>)),
  ?assertEqual({array, [[{<<"name">>, {array, [<<"string_val_1">>, <<"string_val_2">>]}}]]},
    json_util:string_to_emongo(<<"[{\"name\" : [\"string_val_1\", \"string_val_2\"]}]">>)),
  ?assertEqual({array, [[{<<"name">>, {array, [[{<<"name">>, <<"John">>}], [{<<"name">>, <<"Smith">>}]]}}]]},
    json_util:string_to_emongo(<<"[{\"name\" : [{\"name\" : \"John\"}, {\"name\" : \"Smith\"}]}]">>)),
  ?assertEqual({array, [[{<<"address">>, [{<<"street">>, <<"string_val">>}, {<<"zip">>, <<"30332">>}]}, {<<"name">>, <<"John">>}]]},
    json_util:string_to_emongo(<<"[{\"address\" : {\"street\" : \"string_val\", \"zip\" : \"30332\"}, \"name\" : \"John\"}]">>)),
  ok.

emongo_to_string_test() ->
  ?assertEqual(<<"{}">>, json_util:emongo_to_string([])),
  ?assertEqual(<<"[]">>, json_util:emongo_to_string({array, []})),
  ?assertEqual(<<"[{}]">>, json_util:emongo_to_string({array, [[]]})),
  ?assertEqual(<<"[{\"name\":\"string_val\"}]">>, json_util:emongo_to_string({array, [[{<<"name">>, <<"string_val">>}]]})),
  ?assertEqual(<<"[{\"name\":\"\"}]">>, json_util:emongo_to_string({array, [[{<<"name">>, <<"">>}]]})),
  ?assertEqual(<<"[{\"name\":123}]">>, json_util:emongo_to_string({array, [[{<<"name">>, 123}]]})),
  ?assertMatch(<<"[{\"name\":1.1", _/binary>>, json_util:emongo_to_string({array, [[{<<"name">>, 1.1}]]})),
  ?assertEqual(<<"[{\"name\":true}]">>, json_util:emongo_to_string({array, [[{<<"name">>, true}]]})),
  ?assertEqual(<<"[{\"name\":false}]">>, json_util:emongo_to_string({array, [[{<<"name">>, false}]]})),
  ?assertEqual(<<"[{\"name\":null}]">>, json_util:emongo_to_string({array, [[{<<"name">>, undefined}]]})),
  ?assertEqual(<<"[{\"name\":[]}]">>, json_util:emongo_to_string({array, [[{<<"name">>, {array, []}}]]})),
  ?assertEqual(<<"[{\"name\":[\"string_val_1\",\"string_val_2\"]}]">>,
    json_util:emongo_to_string({array, [[{<<"name">>, {array, [<<"string_val_1">>, <<"string_val_2">>]}}]]})),
  ?assertEqual(<<"[{\"name\":[{\"name\":\"John\"},{\"name\":\"Smith\"}]}]">>,
    json_util:emongo_to_string({array, [[{<<"name">>, {array, [[{<<"name">>, <<"John">>}], [{<<"name">>, <<"Smith">>}]]}}]]})),
  ?assertEqual(<<"[{\"address\":{\"street\":\"string_val\",\"zip\":\"30332\"},\"name\":\"John\"}]">>,
    json_util:emongo_to_string({array, [[{<<"address">>, [{<<"street">>, <<"string_val">>}, {<<"zip">>, <<"30332">>}]}, {<<"name">>, <<"John">>}]]})),
  ok.

string_to_rfc4627_test() ->
  ?assertEqual({obj, []}, json_util:string_to_rfc4627(<<"{}">>)),
  ?assertEqual([], json_util:string_to_rfc4627(<<"[]">>)),
  ?assertEqual([{obj, []}], json_util:string_to_rfc4627(<<"[{}]">>)),
  ?assertEqual([{obj, [{"name", <<"string_val">>}]}], json_util:string_to_rfc4627(<<"[{\"name\" : \"string_val\"}]">>)),
  ?assertEqual([{obj, [{"name", <<"">>}]}], json_util:string_to_rfc4627(<<"[{\"name\" : \"\"}]">>)),
  ?assertEqual([{obj, [{"name", 123}]}], json_util:string_to_rfc4627(<<"[{\"name\" : 123}]">>)),
  ?assertEqual([{obj, [{"name", 1.1}]}], json_util:string_to_rfc4627(<<"[{\"name\" : 1.1}]">>)),
  ?assertEqual([{obj, [{"name", true}]}], json_util:string_to_rfc4627(<<"[{\"name\" : true}]">>)),
  ?assertEqual([{obj, [{"name", false}]}], json_util:string_to_rfc4627(<<"[{\"name\" : false}]">>)),
  ?assertEqual([{obj, [{"name", null}]}], json_util:string_to_rfc4627(<<"[{\"name\" : null}]">>)),
  ?assertEqual([{obj, [{"name", []}]}], json_util:string_to_rfc4627(<<"[{\"name\" : []}]">>)),
  ?assertEqual([{obj, [{"name", [<<"string_val_1">>, <<"string_val_2">>]}]}],
    json_util:string_to_rfc4627(<<"[{\"name\" : [\"string_val_1\", \"string_val_2\"]}]">>)),
  ?assertEqual([{obj, [{"name", [{obj, [{"name", <<"John">>}]}, {obj, [{"name", <<"Smith">>}]}]}]}],
    json_util:string_to_rfc4627(<<"[{\"name\" : [{\"name\" : \"John\"}, {\"name\" : \"Smith\"}]}]">>)),
  ?assertEqual([{obj, [{"address", {obj, [{"street", <<"string_val">>}, {"zip", <<"30332">>}]}}, {"name", <<"John">>}]}],
    json_util:string_to_rfc4627(<<"[{\"address\" : {\"street\" : \"string_val\", \"zip\" : \"30332\"}, \"name\" : \"John\"}]">>)),
  ok.

rfc4627_to_string_test() ->
  ?assertEqual(<<"{}">>, json_util:rfc4627_to_string({obj, []})),
  ?assertEqual(<<"[]">>, json_util:rfc4627_to_string([])),
  ?assertEqual(<<"[{}]">>, json_util:rfc4627_to_string([{obj, []}])),
  ?assertEqual(<<"[{\"name\":\"string_val\"}]">>, json_util:rfc4627_to_string([{obj, [{"name", <<"string_val">>}]}])),
  ?assertEqual(<<"[{\"name\":\"\"}]">>, json_util:rfc4627_to_string([{obj, [{"name", <<"">>}]}])),
  ?assertEqual(<<"[{\"name\":123}]">>, json_util:rfc4627_to_string([{obj, [{"name", 123}]}])),
  ?assertMatch(<<"[{\"name\":1.1", _/binary>>, json_util:rfc4627_to_string([{obj, [{"name", 1.1}]}])),
  ?assertEqual(<<"[{\"name\":true}]">>, json_util:rfc4627_to_string([{obj, [{"name", true}]}])),
  ?assertEqual(<<"[{\"name\":false}]">>, json_util:rfc4627_to_string([{obj, [{"name", false}]}])),
  ?assertEqual(<<"[{\"name\":null}]">>, json_util:rfc4627_to_string([{obj, [{"name", null}]}])),
  ?assertEqual(<<"[{\"name\":[]}]">>, json_util:rfc4627_to_string([{obj, [{"name", []}]}])),
  ?assertEqual(<<"[{\"name\":[\"string_val_1\",\"string_val_2\"]}]">>,
    json_util:rfc4627_to_string([{obj, [{"name", [<<"string_val_1">>, <<"string_val_2">>]}]}])),
  ?assertEqual(<<"[{\"name\":[{\"name\":\"John\"},{\"name\":\"Smith\"}]}]">>,
    json_util:rfc4627_to_string([{obj, [{"name", [{obj, [{"name", <<"John">>}]}, {obj, [{"name", <<"Smith">>}]}]}]}])),
  ?assertEqual(<<"[{\"address\":{\"street\":\"string_val\",\"zip\":\"30332\"},\"name\":\"John\"}]">>,
    json_util:rfc4627_to_string([{obj, [{"address", {obj, [{"street", <<"string_val">>}, {"zip", <<"30332">>}]}}, {"name", <<"John">>}]}])),
  ok.

string_to_mongodb_test() ->
  ?assertEqual({}, json_util:string_to_mongodb(<<"{}">>)),
  ?assertEqual([], json_util:string_to_mongodb(<<"[]">>)),
  ?assertEqual([{}], json_util:string_to_mongodb(<<"[{}]">>)),
  ?assertEqual([{name, <<"string_val">>}], json_util:string_to_mongodb(<<"[{\"name\" : \"string_val\"}]">>)),
  ?assertEqual([{name, <<"">>}], json_util:string_to_mongodb(<<"[{\"name\" : \"\"}]">>)),
  ?assertEqual([{name, 123}], json_util:string_to_mongodb(<<"[{\"name\" : 123}]">>)),
  ?assertEqual([{name, 1.1}], json_util:string_to_mongodb(<<"[{\"name\" : 1.1}]">>)),
  ?assertEqual([{name, true}], json_util:string_to_mongodb(<<"[{\"name\" : true}]">>)),
  ?assertEqual([{name, false}], json_util:string_to_mongodb(<<"[{\"name\" : false}]">>)),
  ?assertEqual([{name, null}], json_util:string_to_mongodb(<<"[{\"name\" : null}]">>)),
  ?assertEqual([{name, []}], json_util:string_to_mongodb(<<"[{\"name\" : []}]">>)),
  ?assertEqual([{name, [<<"string_val_1">>, <<"string_val_2">>]}],
    json_util:string_to_mongodb(<<"[{\"name\" : [\"string_val_1\", \"string_val_2\"]}]">>)),
  ?assertEqual([{name, [{name, <<"John">>}, {name, <<"Smith">>}]}],
    json_util:string_to_mongodb(<<"[{\"name\" : [{\"name\" : \"John\"}, {\"name\" : \"Smith\"}]}]">>)),
  ?assertEqual([{address, {street, <<"string_val">>, zip, <<"30332">>}, name, <<"John">>}],
    json_util:string_to_mongodb(<<"[{\"address\" : {\"street\" : \"string_val\", \"zip\" : \"30332\"}, \"name\" : \"John\"}]">>)),
  ok.

mongodb_to_string_test() ->
  ?assertEqual(<<"{}">>, json_util:mongodb_to_string({})),
  ?assertEqual(<<"[]">>, json_util:mongodb_to_string([])),
  ?assertEqual(<<"[{}]">>, json_util:mongodb_to_string([{}])),
  ?assertEqual(<<"[{\"name\":\"string_val\"}]">>, json_util:mongodb_to_string([{name, <<"string_val">>}])),
  ?assertEqual(<<"[{\"name\":\"\"}]">>, json_util:mongodb_to_string([{name, <<"">>}])),
  ?assertEqual(<<"[{\"name\":123}]">>, json_util:mongodb_to_string([{name, 123}])),
  ?assertMatch(<<"[{\"name\":1.1", _/binary>>, json_util:mongodb_to_string([{name, 1.1}])),
  ?assertEqual(<<"[{\"name\":true}]">>, json_util:mongodb_to_string([{name, true}])),
  ?assertEqual(<<"[{\"name\":false}]">>, json_util:mongodb_to_string([{name, false}])),
  ?assertEqual(<<"[{\"name\":null}]">>, json_util:mongodb_to_string([{name, null}])),
  ?assertEqual(<<"[{\"name\":[]}]">>, json_util:mongodb_to_string([{name, []}])),
  ?assertEqual(<<"[{\"name\":[\"string_val_1\",\"string_val_2\"]}]">>,
    json_util:mongodb_to_string([{name, [<<"string_val_1">>, <<"string_val_2">>]}])),
  ?assertEqual(<<"[{\"name\":[{\"name\":\"John\"},{\"name\":\"Smith\"}]}]">>,
    json_util:mongodb_to_string([{name, [{name, <<"John">>}, {name, <<"Smith">>}]}])),
  ?assertEqual(<<"[{\"address\":{\"street\":\"string_val\",\"zip\":\"30332\"},\"name\":\"John\"}]">>,
    json_util:mongodb_to_string([{address, {street, <<"string_val">>, zip, <<"30332">>}, name, <<"John">>}])),
  ok.
