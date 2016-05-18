-module(test_helpers).
-export([
    assert_fail/5,
    check_function_called/3,
    check_list_contains_values/3
]).

-spec assert_fail(fun(), list(), atom(), atom(), atom()) -> any().
assert_fail(Fun, Args, ExceptionType, ExceptionValue, Reason) ->
    try apply(Fun, Args) of
        _ -> ct:fail(Reason)
    catch
        ExceptionType:ExceptionValue -> ok
    end.

-spec check_function_called(atom(), list(), list()) -> boolean().
check_function_called(Module, Function, Params) ->
    lists:keymember({Module, Function, Params}, 2, meck:history(Module)).

-spec check_list_contains_values(list(), list(), list()) -> any().
check_list_contains_values(ExistingValues, AbsentValues, List) ->
    lists:member(true,
        [lists:member(Value, List) == false || Value <- ExistingValues] ++
        [lists:member(Value, List) || Value <- AbsentValues]) == false.