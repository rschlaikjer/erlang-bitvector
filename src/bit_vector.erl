-module(bit_vector).
-export([
    new/1,
    set/3,
    get/2
]).

new(Size) ->
    bitvector_native:vector_new(Size).

set(Vector, Idx, Val) when is_integer(Idx) andalso is_integer(Val) andalso Val >= 0 ->
    bitvector_native:vector_set(Vector, Idx, Val).

get(Buffer, Idx) ->
    bitvector_native:vector_get(Buffer, Idx).
