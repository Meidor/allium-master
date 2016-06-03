-module(test_helpers).

%% API
-export([
    assert_fail/5,
    check_function_called/3,
    check_list_contains_values/3
]).

-spec assert_fail(fun(), list(), atom(), atom(), atom()) -> any().
%% @doc
%% Used to test if a function call will result in a fail.
%% params
%% Fun: the name of the function that is going to be tested.
%% Args: the arguments that will be used as parameters for the function.
%% ExceptionType: the type of error that is expected to show up.
%% ExceptionValue: the value of the exception that will be thrown.
%% Reason: the reason of the exception.
assert_fail(Fun, Args, ExceptionType, ExceptionValue, Reason) ->
    try apply(Fun, Args) of
        _ -> ct:fail(Reason)
    catch
        ExceptionType:ExceptionValue -> ok
    end.

-spec check_function_called(atom(), list(), list()) -> boolean().
%% @doc
%% Used to check if a function is called.
%% params
%% Module: the name of the module where the called on function is implemented.
%% Function: the name of the function that is going to be tested.
%% Params: the parameters for the function.
check_function_called(Module, Function, Params) ->
    lists:keymember({Module, Function, Params}, 2, meck:history(Module)).

-spec check_list_contains_values(list(), list(), list()) -> any().
%% @doc
%% Used to check if a list contains the given values and does not contain other given values.
%% params
%% ExistingValues: the values that are expected to show up in the list.
%% AbsentValues: the values that are expected to not show up in the list.
%% List: a list that contains certain values.
check_list_contains_values(ExistingValues, AbsentValues, List) ->
    lists:member(true,
        [lists:member(Value, List) == false || Value <- ExistingValues] ++
        [lists:member(Value, List) || Value <- AbsentValues]) == false.