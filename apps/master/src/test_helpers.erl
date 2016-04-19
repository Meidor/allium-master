-module(test_helpers).
-export([assert_fail/5, check_function_called/3]).

assert_fail(Fun, Args, ExceptionType, ExceptionValue, Reason) ->
  try apply(Fun, Args) of
    _ -> ct:fail(Reason)
  catch
    ExceptionType:ExceptionValue -> ok
  end.

check_function_called(Module, Function, Params) ->
  FunctionsCalled = [{CalledModule, CalledFunction, CalledParams} || {_, {CalledModule, CalledFunction, CalledParams}, Result}
    <- meck:history(Module), CalledFunction =:= Function, CalledModule =:= Module, CalledParams =:= Params, Result =:= ok],
  case FunctionsCalled of
     [] ->
      false;
    _ ->
      true
  end.


