-module(test_helpers).
-export([assert_fail/5]).

assert_fail(Fun, Args, ExceptionType, ExceptionValue, Reason) ->
  try apply(Fun, Args) of
    _ -> ct:fail(Reason)
  catch
    ExceptionType:ExceptionValue -> ok
  end.