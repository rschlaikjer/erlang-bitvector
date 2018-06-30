Bit Vectors + Ring Buffers for Erlang
=====

Native C implementation of bit vectors, and bit ringbuffers.

Add as a dependency:

```erlang
{bitvector, "0.4.0", {git, "https://github.com/rschlaikjer/erlang-bitvector.git", {tag, "0.4.0"}}}
```

Bit vectors simply store packed bit values:

```
1> {ok, V2ec} = bit_vector:new(10).
{ok, <<>>}
2> bit_vector:set(Vec, 0, 1).
ok
3> bit_vector:get(Vec, 0).
{ok, 1}
4> bit_vector:set(Vec, 10, 1).
{error,index_out_of_range}
```

Ringbuffers are useful for tracking set numbers of boolean events:
```
1> {ok, R} = bit_ringbuffer:new(3).
<<>>
2> bit_ringbuffer:popcnt(R).
{ok, 0}
3> [bit_ringbuffer:append(R, 1) || _ <- lists:seq(1, 3)].
[ok,ok,ok]
4> bit_ringbuffer:popcnt(R).
{ok, 3}
5> bit_ringbuffer:append(R, 0).
ok
6> bit_ringbuffer:popcnt(R).
{ok, 2}
```

## API reference

### bit_vector

#### new(Size) -> {ok, Vector} | {error, Reason}

- **Size = pos_integer()**
- **Vector = bit_vector:bit_vector()**
- **Reason = atom()**

Returns a new bit vector, with a capacity of **Size** bits. If the **Size**
parameter is not a positive integer, an error tuple will be returned
instead.

#### set(Vector, Index, Value) -> ok | {error, Reason}

- **Vector = bit_vector:bit_vector()**
- **Index = non_neg_iteger()**
- **Value = non_neg_iteger()**
- **Reason = atom()**

Sets the bit at position **Index** in vector **Vector** to `1` if
**Value** is greater than zero, or `0` if **Value** is equal to zero.
Returns `ok` on success, or an error if the index or value are invalid.

#### get(Vector, Index) -> {ok, Bit} | {error, Reason}

 - **Vector = bit_vector:bit_vector()**
 - **Index = non_neg_iteger()**
 - **Bit = 0 | 1**
 - **Reason = atom()**

Retrieve the bit at position **Index** in vector **Vector**. The value for
**Bit** will be either `0` or `1` on success.
An error tuple is returned if **Index** is not valid.

### bit_ringbuffer

#### new(Size) -> {ok, Buffer} | {error, Reason}

- **Size = pos_integer()**
- **Buffer = bit_ringbuffer:bit_ringbuffer()**
- **Reason = atom()**

Returns a new ring buffer, with a capacity of **Size** bits. If the **Size**
parameter is not a positive integer, an error tuple will be returned
instead.

#### append(Buffer, Value) -> ok | {error, Reason}

- **Buffer = bit_ringbuffer:bit_ringbuffer()**
- **Value = non_neg_integer()**
- **Reason = atom()**

Append a value **Value** to the ringbuffer **Buffer**. If **Value** is zero, a
`0` bit is appended. For any positive integer, a `1` bit is appended.
An error tuple is returned if **Buffer** is not a valid ringbuffer.

#### popcnt(Buffer) -> {ok, Count} | {error, Reason}

- **Buffer = bit_ringbuffer:bit_ringbuffer()**
- **Count = non_neg_integer()**
- **Reason = atom()**

Get the **Count** of set bits in the ring buffer **Buffer**.
An error tuple is returned if **Buffer** is not a valid ringbuffer.

# License

Copyright 2018 Ross Schlaikjer

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
