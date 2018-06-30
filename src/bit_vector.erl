-module(bit_vector).
-export([
    new/1,
    set/3,
    get/2
]).
-export_type([bit_vector/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type bit_vector() :: binary(). % Native references appear to be binaries

-spec new(pos_integer()) -> {ok, bit_vector()} | {error, atom()}.
new(Size) ->
    bitvector_native:vector_new(Size).

-spec set(bit_vector(), non_neg_integer(), non_neg_integer()) -> ok | {error, atom()}.
set(Vector, Idx, Val) when is_integer(Idx) andalso is_integer(Val) andalso Val >= 0 ->
    bitvector_native:vector_set(Vector, Idx, Val).

-spec get(bit_vector(), non_neg_integer()) -> {ok, non_neg_integer()} | {error, atom()}.
get(Buffer, Idx) when is_integer(Idx) ->
    bitvector_native:vector_get(Buffer, Idx).

-ifdef(TEST).

negative_size_test() ->
    {error, bad_size} = new(-1).

zero_size_test() ->
    {error, bad_size} = new(0).

initialized_to_zero_test() ->
    {ok, Vec} = new(1000),
    true = lists:all(
        fun(V) -> case V of {ok, 0} -> true; _ -> false end end,
        [get(Vec, I) || I <- lists:seq(1, 999)]
    ).

single_word_test() ->
    {ok, Vec} = new(10),
    [set(Vec, I, 1) || I <- lists:seq(0, 9)],
    true = lists:all(
        fun(V) -> case V of {ok, 1} -> true; _ -> false end end,
        [get(Vec, I) || I <- lists:seq(0, 9)]
    ).

multi_word_test() ->
    {ok, Vec} = new(1000),
    [set(Vec, I, 1) || I <- lists:seq(0, 999)],
    true = lists:all(
        fun(V) -> case V of {ok, 1} -> true; _ -> false end end,
        [get(Vec, I) || I <- lists:seq(0, 999)]
    ).

multi_word_pattern_test() ->
    {ok, Vec} = new(1000),
    [set(Vec, I, I rem 2) || I <- lists:seq(0, 999)],
    500 = lists:foldl(
        fun({ok, V}, Acc) -> Acc + V end,
        0,
        [get(Vec, I) || I <- lists:seq(0, 999)]
    ).

negative_index_set_test() ->
    {ok, Vec} = new(10),
    {error, bad_index} = set(Vec, -1, 1).

too_large_index_set_test() ->
    {ok, Vec} = new(10),
    {error, index_out_of_range} = set(Vec, 10, 1).

negative_index_get_test() ->
    {ok, Vec} = new(10),
    {error, bad_index} = get(Vec, -1).

too_large_index_get_test() ->
    {ok, Vec} = new(10),
    {error, index_out_of_range} = get(Vec, 10).

non_integer_index_set_test() ->
    {ok, Vec} = new(10),
    ?assertError(function_clause, set(Vec, 1.1, 1)),
    ?assertError(function_clause, set(Vec, true, 1)),
    ?assertError(function_clause, set(Vec, <<>>, 1)).

non_integer_index_get_test() ->
    {ok, Vec} = new(10),
    ?assertError(function_clause, get(Vec, 1.1)),
    ?assertError(function_clause, get(Vec, true)),
    ?assertError(function_clause, get(Vec, <<>>)).

-endif.
