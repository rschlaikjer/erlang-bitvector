Bit Vectors + Ring Buffers for Erlang
=====

Native C implementation of bit vectors, and bit ringbuffers.

Add as a dependency:

```erlang
{bitvector, "0.2.0", {git, "https://github.com/rschlaikjer/erlang-bitvector.git", {tag, "0.2.0"}}}
```

Bit vectors simply store packed bit values:

```
1> Vec = bit_vector:new(10).
<<>>
2> bit_vector:set(Vec, 0, 1).
ok
3> bit_vector:get(Vec, 0).
1
4> bit_vector:set(Vec, 10, 1).
{error,index_out_of_range}
```

Ringbuffers are useful for tracking set numbers of boolean events:
```
1> R = bit_ringbuffer:new(3).
<<>>
2> bit_ringbuffer:popcnt(R).
0
3> [bit_ringbuffer:append(R, 1) || _ <- lists:seq(1, 3)].
[ok,ok,ok]
4> bit_ringbuffer:popcnt(R).
3
5> bit_ringbuffer:append(R, 0).
ok
6> bit_ringbuffer:popcnt(R).
2
```

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
