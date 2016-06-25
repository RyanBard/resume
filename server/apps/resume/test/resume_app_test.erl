-module(resume_app_test).

-author('Ryan Bard <john.ryan.bard@gmail.com>').

-include("resume_test.hrl").

-compile(export_all).


start_and_stop_test() ->
  ?assertEqual(false, is_process_registered(resume_sup)),
  rh:start(), % TODO - keep this in a "common utility" (convenient hack) or duplicate that code in this test?
  ?assertEqual(true, is_process_registered(resume_sup)),
  application:stop(resume),
  ok = timer:sleep(1000), % I'm making sure the supervisor isn't restarted shortly after the asserts pass.
  ?assertEqual(false, is_process_registered(resume_sup)),
  ok.


is_process_registered(ProcessName) ->
  whereis(ProcessName) =/= undefined.
