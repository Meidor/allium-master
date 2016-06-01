-module(test_helpers).
-export([
    assert_fail/5,
    check_function_called/3,
    check_list_contains_values/3
]).

%% @doc
%% used to test if a function call will result in a fail.
-spec assert_fail(fun(), list(), atom(), atom(), atom()) -> any().
assert_fail(Fun, Args, ExceptionType, ExceptionValue, Reason) ->
    try apply(Fun, Args) of
        _ -> ct:fail(Reason)
    catch
        ExceptionType:ExceptionValue -> ok
    end.

%% @doc
%% used to check if a function is called.
-spec check_function_called(atom(), list(), list()) -> boolean().
check_function_called(Module, Function, Params) ->
    lists:keymember({Module, Function, Params}, 2, meck:history(Module)).

%% @doc
%%  used to chech if a value is contained in a list.
-spec check_list_contains_values(list(), list(), list()) -> any().
check_list_contains_values(ExistingValues, AbsentValues, List) ->
    lists:member(true,
        [lists:member(Value, List) == false || Value <- ExistingValues] ++
        [lists:member(Value, List) || Value <- AbsentValues]) == false.