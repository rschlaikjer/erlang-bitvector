#ifndef BITVECTOR_H
#define BITVECTOR_H

#define _POSIX_C_SOURCE 199309L

#include <string.h>
#include <stdint.h>

#include "erl_nif.h"

// For hushing compiler warnings
#define UNUSED __attribute__((unused))

struct PrivData {
    // Resource type for allocated bit vector data
    ErlNifResourceType *bit_data_resource;
    // Bit-vector state type
    ErlNifResourceType *bit_vector_state_resource;
    // Bit-ringbuffer state type
    ErlNifResourceType *bit_ringbuffer_state_resource;
};

struct bit_vector {
    // Size (bits)
    uint64_t vector_size;
    // Actual data
    uint64_t *bit_data;
};

struct bit_ringbuffer {
    // Size (bits)
    uint64_t vector_size;
    // Actual data
    uint64_t *bit_data;
    // uint64_t offset into bit_data
    uint64_t word_offset;
    // Individual bit offset within selected 'word'
    uint8_t word_index;
};

// Plain vectors
static ERL_NIF_TERM erl_bitvector_new(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);
static ERL_NIF_TERM erl_bitvector_set(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);
static ERL_NIF_TERM erl_bitvector_get(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);

// Ringbuffers
static ERL_NIF_TERM erl_ringbuffer_new(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);
static ERL_NIF_TERM erl_ringbuffer_append(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);
static ERL_NIF_TERM erl_ringbuffer_popcnt(
    ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
);

// Internal
static uint64_t popcnt_vector(uint64_t* vector, uint64_t vector_size_bits);

// Internal term manipulation
ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);

// Module callbacks
int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
void unload(ErlNifEnv* env, void* priv_data);

// Export methods
static ErlNifFunc nif_funcs[] = {
    {"vector_new", 1, erl_bitvector_new, 0},
    {"vector_get", 2, erl_bitvector_get, 0},
    {"vector_set", 3, erl_bitvector_set, 0},
    {"ringbuffer_new", 1, erl_ringbuffer_new, 0},
    {"ringbuffer_append", 2, erl_ringbuffer_append, 0},
    {"ringbuffer_popcnt", 1, erl_ringbuffer_popcnt, 0}
};
ERL_NIF_INIT(bitvector, nif_funcs, load, NULL, upgrade, unload);

#endif // BITVECTOR_H
