-module(bit_ringbuffer).
-export([
    new/1,
    append/2,
    popcnt/1
]).
-export_type([bit_ringbuffer/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type bit_ringbuffer() :: binary(). % Native references appear to be binaries

-spec new(pos_integer()) -> {ok, bit_ringbuffer()} | {error, atom()}.
new(Size) ->
    bitvector_native:ringbuffer_new(Size).

-spec append(bit_ringbuffer(), non_neg_integer()) -> ok | {error, atom()}.
append(Buffer, Val) when is_integer(Val) andalso Val >= 0 ->
    bitvector_native:ringbuffer_append(Buffer, Val).

-spec popcnt(bit_ringbuffer()) -> {ok, non_neg_integer()} | {error, atom()}.
popcnt(Buffer) ->
    bitvector_native:ringbuffer_popcnt(Buffer).

-ifdef(TEST).

negative_size_test() ->
    {error, bad_size} = new(-1).

zero_size_test() ->
    {error, bad_size} = new(0).

initialized_to_zero_test() ->
    {ok, Buf} = new(1000),
    {ok, 0} = popcnt(Buf).

single_word_test() ->
    {ok, Buf} = new(10),
    {ok, 0} = popcnt(Buf),
    [append(Buf, 1) || _ <- lists:seq(1, 10)],
    {ok, 10} = popcnt(Buf).

multi_word_test() ->
    {ok, Buf} = new(1000),
    {ok, 0} = popcnt(Buf),
    [append(Buf, 1) || _ <- lists:seq(1, 100)],
    {ok, 100} = popcnt(Buf),
    [append(Buf, 1) || _ <- lists:seq(1, 900)],
    {ok, 1000} = popcnt(Buf).

multi_word_pattern_test() ->
    {ok, Buf} = new(1000),
    [append(Buf, I rem 2) || I <- lists:seq(0, 999)],
    {ok, 500} = popcnt(Buf).

-endif.
