-module(example_tests).
-include_lib("eunit/include/eunit.hrl").

% <http://erlang.org/doc/apps/eunit/chapter.html>
% <https://github.com/richcarl/eunit/blob/master/examples/eunit_examples.erl>
% <http://learnyousomeerlang.com/eunit#test-generators>

fib(0) -> 1;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).
fib_test_() ->
  {"Fib",
    [{"works for some inputs",
      [?_assertEqual(1, fib(0)),
       ?_assertEqual(1, fib(1)),
       ?_assertEqual(2, fib(2)),
       ?_assertEqual(3, fib(3)),
       ?_assertEqual(5, fib(4)),
       ?_assertEqual(8, fib(5))]},
     {"fib(31)", ?_assertEqual(2178309, fib(31))},
     {"fib(-1) errors", ?_assertException(error, function_clause, fib(-1))}]}.


addition_test_() ->
  {"tests", [{"add(1, 1)", ?_assertEqual(2, solution:add(1, 1))},
             {"add(1, 2)", ?_assertEqual(3, solution:add(1, 2))}]}.

succeed() -> ok.
fail() -> throw(failed).

succeeding_test() ->
  succeed().
failing_test() ->
  fail().

succeeding_fun_test_() ->
  fun () -> ok end.
failing_fun_test_() ->
  fun () -> throw(failed) end.

succeeding_simple_test_() -> ?_test(succeed()).
failing_simple_test_() -> ?_test(fail()).

succeeding_assert_test_() -> ?_assert(1 > 0).
failing_assert_test_() -> ?_assert(0 > 1).

succeeding_error_test_() -> ?_assertError(foo, erlang:error(foo)).
failing_error_test_() -> ?_assertError(foo, erlang:throw(foo)).

succeeding_exit_test_() -> ?_assertExit(foo, erlang:exit(foo)).
failing_exit_test_() -> ?_assertExit(foo, erlang:throw(foo)).

succeeding_throw_test_() -> ?_assertThrow(foo, erlang:throw(foo)).
failing_throw_test_() -> ?_assertThrow(foo, erlang:exit(foo)).

succeeding_named_test_() ->
  {"A Test with a Title", ?_test(succeed())}.
failing_named_test_() ->
  {"A Failing Test with a Title", ?_test(fail())}.

succeeding_wrapper_test_() ->
  {"A Wrapped Function", fun succeeding_test/0}.
failing_wrapper_test_() ->
  {"A failing Wrapped Function", fun failing_test/0}.

empty_list_test_() ->
    []. %% should be accepted, but will not produce any visible result

list_test_() ->
    [?_test(succeed()),
     ?_test(succeed()),
     ?_test(succeed())].

% the following
% [?_test(succeed()),
%  [[],
%   ?_test(fail())],
%  ?_test(succeed()),
%  []].
% becomes
% [?_test(succeed()),
%  ?_test(fail()),
%  ?_test(succeed())].
deep_list_test_() ->
    [?_test(succeed()),
     [[],
      ?_test(succeed())],
     ?_test(succeed()),
     []].

deep_list2_test_() ->
    [?_test(succeed()),
     [?_test(succeed()),
      ?_test(succeed())],
     ?_test(succeed()),
     []].

list_of_named_test_() ->
  [{"Test 1", ?_test(succeed())},
   {"Test 2", ?_test(succeed())},
   {"Test 3", ?_test(succeed())}].


group_test_() ->
  {"Group of Tests",
   [?_test(succeed()),
    ?_test(succeed()),
    ?_test(succeed())]}.

named_group_test_() ->
  {"Group of Tests with Titles",
   [{"Subtest 1", ?_test(succeed())},
    {"Subtest 2", ?_test(succeed())},
    {"Subtest 3", ?_test(succeed())}]}.

nested_named_list_test_() ->
  {"Named List Level One",
   [?_test(succeed()),
    {"Named List Level Two",
     [?_test(succeed()),
      ?_test(succeed()),
      ?_test(succeed())]},
    ?_test(succeed()),
    ?_test(succeed())]
  }.

fixture_setup_test_() ->
  {"Setup",
   {setup,
    fun () -> 1 end,
    fun (X) ->
        [{"setup 1", ?_assert(X =:= 1)},
         {"setup 2", ?_assert(X =:= 1)},
         {"setup 3", ?_assert(X =:= 1)}]
    end}
  }.

foreach_test_() ->
  {"Foreach",
   {foreach,
    fun () -> 1 end, % setup
    [fun (R) -> {"foreach 1", ?_assert(R =:= 1)} end,
     fun (R) -> {"foreach 2", ?_assert(R =:= 1)} end,
     fun (R) -> {"foreach 3", ?_assert(R =:= 1)} end]}}. % tests

foreachx_test_() ->
  {"ForeachX",
   {foreachx,
    fun (X) -> 1 + X end,
    [{1, fun (X, R) -> {"foreach X 1", ?_assertEqual(3, X + R)} end},
     {2, fun (X, R) -> {"foreach X 2", ?_assertEqual(5, X + R)} end},
     {3, fun (X, R) -> {"foreach X 3", ?_assertEqual(7, X + R)} end}]}}.

with_test_() ->
  {"With",
   {with, 1,
    [fun (Y) -> ?assertEqual(1, Y) end,
     fun (Y) -> ?assertEqual(1, Y) end,
     fun (Y) -> ?assertEqual(2, Y) end]}}.

order_test_() ->
  {"Order",
   {inparallel,
    [{"P1", inorder,
      [{"put 1",  ?_test(undefined = put(foo, 1))},
       {"get 1",  ?_test(1 = get(foo))}]},
     {"P2", inorder,
      [{"put 2",  ?_test(undefined = put(foo, 2))},
       {"get 2",  ?_test(2 = get(foo))}]},
     {"P3", inorder,
      [{"put 3",  ?_test(undefined = put(foo, 3))},
       {"fail", ?_test(exit(foo))},
       {"get 3",  ?_test(3 = get(foo))}]}]}}.

spawn_test_() ->
  {"Spawn",
    {"Level 1", inorder,
     [{"put 1", ?_test(undefined = put(foo, 1))},
      {"Level 2", spawn,
       [{"put 2", ?_test(undefined = put(foo, 2))}, % new process
        {"Level 3", spawn,
         [{"put 3", ?_test(undefined = put(foo, 3))},
          {"fail", ?_test(exit(foo))},
          {"get 3", ?_test(3 = get(foo))}
         ]},
        {"get 2", ?_test(2 = get(foo))}
       ]},
      {"get 1", ?_test(1 = get(foo))}
     ]}}.

timeout_test_() ->
  {"Timeout", spawn, {timeout, 1, ?_test(receive after 2000 -> ok end)}}.

setup_timeout_test_() ->
  {"Setup timeout", setup,
   fun () -> erlang:display(setup)  end,
   fun (_) -> erlang:display(cleanup) end,
   fun (_) -> {timeout, 1, ?_test(receive after 2000 -> ok end)} end}.

foreach_timeout_test_() ->
  {"Foreach Timeout", foreach,
   fun () -> erlang:display(setup) end,
   fun (_) -> erlang:display(cleanup) end,
   [?_test(succeed()),
    {timeout, 1, ?_test(receive after 2000 -> ok end)},
    ?_test(succeed())]}.
