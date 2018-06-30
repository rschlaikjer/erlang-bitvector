-module(bit_ringbuffer).
-export([
    new/1,
    append/2,
    popcnt/1
]).

new(Size) ->
    bitvector_native:ringbuffer_new(Size).

append(Buffer, Val) when is_integer(Val) andalso Val >= 0 ->
    bitvector_native:ringbuffer_append(Buffer, Val).

popcnt(Buffer) ->
    bitvector_native:ringbuffer_popcnt(Buffer).
