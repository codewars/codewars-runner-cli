-module(eunit_codewars).

-behaviour(eunit_listener).

-export([start/0, start/1]).
% eunit_listener
-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2]).

-record(state, {}).

-type state() :: #state{}.


%%% Public

-spec start() -> pid().
start() ->
  start([]).

-spec start(proplists:proplist()) -> pid().
start(Options) ->
  eunit_listener:start(?MODULE, Options).

-spec init(proplists:proplist()) -> state().
init(_Options) ->
  receive
    {start, _Reference} -> #state{}
  end.


-spec handle_begin(group|test, proplists:proplist(), state()) -> state().
handle_begin(group, Data, St) ->
  case is_test_group(Data) of
    true ->
      Desc = proplists:get_value(desc, Data),
      io:fwrite("\n<DESCRIBE::>~ts\n", [esc(Desc)]),
      St;
    false -> St
  end;
handle_begin(test, Data, St) ->
  Desc = case proplists:get_value(desc, Data) of
    undefined -> "Unnamed Test";
    X -> X
  end,
  io:fwrite("\n<IT::>~ts\n", [esc(Desc)]),
  St.

-spec handle_end(group|test, proplists:proplist(), state()) -> state().
handle_end(group, Data, St) ->
  case is_test_group(Data) of
    true ->
      io:fwrite("\n<COMPLETEDIN::>~w\n", [get_time(Data)]),
      St;
    false -> St
  end;
handle_end(test, Data, St) ->
  case proplists:get_value(output, Data) of
    <<>> -> ok;
    undefined -> ok;
    Output -> io:fwrite("~ts\n", [Output])
  end,
  io:fwrite("\n~ts\n", [format_test_status(proplists:get_value(status, Data))]),
  io:fwrite("\n<COMPLETEDIN::>~w\n", [get_time(Data)]),
  St.

-spec handle_cancel(group|test, proplists:proplist(), state()) -> state().
handle_cancel(group, Data, St) ->
  case proplists:get_value(reason, Data) of
    undefined ->
      ok;
    {blame, _} -> % subtask timed out
      io:fwrite("\n<COMPLETEDIN::>0\n", []),
      ok;
    Reason ->
      io:fwrite("\n~ts\n", [format_cancel(Reason)])
  end,
  St;
handle_cancel(test, Data, St) ->
  case proplists:get_value(output, Data) of
    <<>> -> ok;
    undefined -> ok;
    Output -> io:fwrite("~ts\n", [Output])
  end,
  io:fwrite("\n~ts\n", [format_cancel(proplists:get_value(reason, Data))]),
  io:fwrite("\n<COMPLETEDIN::>~w\n", [get_time(Data)]),
  St.

-spec terminate({ok, proplists:proplist()}|{error, {atom(), term(), term()}}, state()) -> any().
terminate({ok, Data}, _St) ->
  case {proplists:get_value(fail, Data, 0), proplists:get_value(skip, Data, 0), proplists:get_value(cancel, Data, 0)} of
    {0, 0, 0} -> sync_end(ok);
    _         -> sync_end(error)
  end,
  ok;
terminate({error, Reason}, _St) ->
  io:fwrite("\n~ts\n", [format_terminate_error(Reason)]),
  sync_end(error),
  ok.



%%% Private

sync_end(Result) ->
  receive
    {stop, Reference, ReplyTo} ->
      ReplyTo ! {result, Reference, Result},
      ok
  end.

is_test_group(Data) ->
  case {proplists:get_value(id, Data), proplists:get_value(desc, Data)} of
    % root group
    {[], _} -> false;
    % internal groups
    {[_], <<"file ",_/binary>>} -> false;
    {_, <<"module '",_/binary>>} -> false;
    {[_,_], <<"application ",_/binary>>} -> false;
    % ignore unnamed groups as it's hard to differentiate from groups created by macros
    {_, undefined} -> false;
    _ -> true
  end.

format_test_status(ok) ->
  ["<PASSED::>Test Passed"];
% Maybe implement specific formatters for each assertion macros, but some are inconsistent.
format_test_status({error, {error, {Term, Info}, _Trace}}) ->
  [esc(io_lib:fwrite("<FAILED::>Test Failed: ~tp\n~tp", [Term, Info]))];
format_test_status({error, {throw, Term, Trace}}) ->
  [esc(io_lib:fwrite("<ERROR::>Test Threw: ~tp\n~ts", [Term, format_stacktrace(Trace)]))];
format_test_status({error, {exit, Term, Trace}}) ->
  [esc(io_lib:fwrite("<ERROR::>Test Exited: ~tp\n~ts", [Term, format_stacktrace(Trace)]))];
format_test_status({skipped, Reason}) ->
  [esc(io_lib:fwrite("<ERROR::>Test Skipped: ~ts", [format_skipped(Reason)]))];
format_test_status(Term) ->
  [esc(io_lib:fwrite("<ERROR::>Unknown Status: ~tp", [Term]))].


format_skipped({module_not_found, M}) ->
  [io_lib:fwrite("Missing module: ~w", [M]), ""];
format_skipped({no_such_function, {M, F, A}}) ->
  [io_lib:fwrite("No such function: ~w:~w/~w", [M, F, A]), ""].

format_cancel(undefined) ->
  ["<FAILED::>Test Skipped"];
format_cancel(timeout) ->
  ["<FAILED::>Test Timed Out"];
format_cancel({startup, Reason}) ->
  [esc(io_lib:fwrite("<ERROR::>Failed to start test process\n~tP", [Reason, 15]))];
format_cancel({blame, _SubId}) ->
  ["<FAILED::>Test cancelled because of subtask"];
format_cancel({exit, Reason}) ->
  [esc(io_lib:fwrite("<ERROR::>Unexpected Termination\n~tP", [Reason, 15]))];
format_cancel({abort, Reason}) ->
  [esc(io_lib:fwrite("<ERROR::>Internal Error\n~ts", [eunit_lib:format_error(Reason)]))].


format_terminate_error({Class, Term, Trace}) ->
  [io_lib:fwrite("<ERROR::>Internal Error\n", []),
   esc(io_lib:fwrite("<LOG::Error>~tw ~tw\n~ts", [Class, Term, format_stacktrace(Trace)]))].


esc(Text) when is_binary(Text) -> esc(binary_to_list(Text));
esc(Text) -> re:replace(Text, "\n", "<:LF:>", [{return,list},global]).

get_time(Data) ->
  case proplists:get_value(time, Data) of
    undefined -> 0;
    T -> T
  end.



% borrowing from 'eunit_lib' so that we can increase the depth for ~P as necessary
format_stacktrace(Trace) ->
  format_stacktrace(Trace, "in function", "in call from").

format_stacktrace([{M,F,A,L}|Fs], Pre, Pre1) when is_integer(A) ->
  [io_lib:fwrite("~ts ~w:~w/~w~ts\n", [Pre, M, F, A, format_stacktrace_location(L)])
   | format_stacktrace(Fs, Pre1, Pre1)];
format_stacktrace([{M,F,As,L}|Fs], Pre, Pre1) when is_list(As) ->
  A = length(As),
  C = case is_op(M,F,A) of
        true when A =:= 1 ->
          [A1] = As,
          io_lib:fwrite("~ts ~ts", [F,format_arg(A1)]);
        true when A =:= 2 ->
          [A1, A2] = As,
          io_lib:fwrite("~ts ~ts ~ts", [format_arg(A1),F,format_arg(A2)]);
        false ->
          io_lib:fwrite("~w(~ts)", [F,format_arglist(As)])
      end,
  [io_lib:fwrite("~ts ~w:~w/~w~ts\n  called as ~ts\n",
                 [Pre,M,F,A,format_stacktrace_location(L),C])
   | format_stacktrace(Fs,Pre1,Pre1)];
format_stacktrace([{M,F,As}|Fs], Pre, Pre1) ->
  format_stacktrace([{M,F,As,[]}|Fs], Pre, Pre1);
format_stacktrace([],_Pre,_Pre1) ->
  "".

format_stacktrace_location(Location) ->
  File = proplists:get_value(file, Location),
  Line = proplists:get_value(line, Location),
  if File =/= undefined, Line =/= undefined ->
       io_lib:format(" (~ts, line ~w)", [File, Line]);
     true ->
       ""
  end.

format_arg(A) ->
  io_lib:format("~tP",[A,15]).

format_arglist([A]) ->
  format_arg(A);
format_arglist([A|As]) ->
  [io_lib:format("~tP,", [A,15]) | format_arglist(As)];
format_arglist([]) ->
  "".

is_op(erlang, F, A) ->
  erl_internal:arith_op(F, A)
  orelse erl_internal:bool_op(F, A)
  orelse erl_internal:comp_op(F, A)
  orelse erl_internal:list_op(F, A)
  orelse erl_internal:send_op(F, A);
is_op(_M, _F, _A) ->
  false.
