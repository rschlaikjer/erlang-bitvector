#include "bitvector.h"

ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom) {
    ERL_NIF_TERM ret;
    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom);
    }
    return ret;
}

ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg) {
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM erl_bitvector_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Assert we got one arg
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    // Extract the int value for the vector size (in bits)
    uint64_t size;
    if(!enif_get_uint64(env, argv[0], &size) || !size) {
        return mk_error(env, "bad_size");
    }

    ErlNifResourceType *bit_data_resource;
    ErlNifResourceType *bit_vector_state_resource;

    // Figure out how many uint64 words we need to store this vector
    uint64_t word_count = (size / 64) + (size % 64 > 0 ? 1 : 0);

    // Alloc the bit vector
    struct PrivData *priv_data = enif_priv_data(env);
    uint64_t *bit_data = enif_alloc_resource(
        priv_data->bit_data_resource,
        word_count * 8
    );

    // Initialize to zero
    memset(bit_data, 0, word_count * 8);

    // Wrap struct
    struct bit_vector* state = enif_alloc_resource(
        priv_data->bit_vector_state_resource,
        sizeof(struct bit_vector)
    );
    state->vector_size = size;
    state->bit_data = bit_data;

    // Return
    ERL_NIF_TERM ret = enif_make_resource(env, state);
    enif_release_resource(state);
    return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
}

static ERL_NIF_TERM erl_bitvector_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) {
        return enif_make_badarg(env);
    }

    // Grab the vector
    struct PrivData *priv_data = enif_priv_data(env);
    struct bit_vector *vector;
    if (!enif_get_resource(env, argv[0],
                           priv_data->bit_vector_state_resource,
                           ((void*) (&vector)))) {
        return mk_error(env, "bad_vector");
    }

    // Get the set index
    uint64_t bit_index;
    if (!enif_get_uint64(env, argv[1], &bit_index)) {
        return mk_error(env, "bad_index");
    }

    // Check if it's in range
    if (bit_index >= vector->vector_size) {
        return mk_error(env, "index_out_of_range");
    }

    // Get the set bit
    unsigned int bit;
    if (!enif_get_uint(env, argv[2], &bit)) {
        return mk_error(env, "bad_value");
    }

    // Set/clear the bit
    const uint64_t word_offset = bit_index / 64;
    const uint8_t word_index = bit_index % 64;
    if (bit) {
        vector->bit_data[word_offset] |= (1 << word_index);
    } else {
        vector->bit_data[word_offset] &= ~(1 << word_index);
    }

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM erl_bitvector_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    // Grab the vector
    struct PrivData *priv_data = enif_priv_data(env);
    struct bit_vector *vector;
    if (!enif_get_resource(env, argv[0],
                           priv_data->bit_vector_state_resource,
                           ((void*) (&vector)))) {
        return mk_error(env, "bad_vector");
    }

    // Get the index
    uint64_t bit_index;
    if (!enif_get_uint64(env, argv[1], &bit_index)) {
        return mk_error(env, "bad_index");
    }

    // Check if it's in range
    if (bit_index >= vector->vector_size) {
        return mk_error(env, "index_out_of_range");
    }

    // Get the bit value
    const uint64_t word_offset = bit_index / 64;
    const uint8_t word_index = bit_index % 64;
    uint8_t value = vector->bit_data[word_offset] & (1 << word_index) ? 1 : 0;

    ERL_NIF_TERM ret = enif_make_uint64(env, value);
    return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
}

static ERL_NIF_TERM erl_ringbuffer_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Assert we got one arg
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    // Extract the int value for the vector size (in bits)
    uint64_t size;
    if(!enif_get_uint64(env, argv[0], &size) || !size) {
        return mk_error(env, "bad_size");
    }

    ErlNifResourceType *bit_data_resource;
    ErlNifResourceType *bit_vector_state_resource;

    // Figure out how many uint64 words we need to store this vector
    uint64_t word_count = (size / 64) + (size % 64 > 0 ? 1 : 0);

    // Alloc the bit vector
    struct PrivData *priv_data = enif_priv_data(env);
    uint64_t *bit_data = enif_alloc_resource(
        priv_data->bit_data_resource,
        word_count * 8
    );

    // Initialize to zero
    memset(bit_data, 0, word_count * 8);

    // Wrap struct
    struct bit_ringbuffer* state = enif_alloc_resource(
        priv_data->bit_ringbuffer_state_resource,
        sizeof(struct bit_ringbuffer)
    );
    state->vector_size = size;
    state->bit_data = bit_data;
    state->word_offset = 0;
    state->word_index = 0;

    // Return
    ERL_NIF_TERM ret = enif_make_resource(env, state);
    enif_release_resource(state);
    return enif_make_tuple2(env, mk_atom(env, "ok"), ret);
}

static ERL_NIF_TERM erl_ringbuffer_append(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Assert we got 2 args, buffer + new value
    if(argc != 2) {
        return enif_make_badarg(env);
    }

    // Grab the ringbuffer state
    struct PrivData *priv_data = enif_priv_data(env);
    struct bit_ringbuffer *buffer;
    if (!enif_get_resource(env, argv[0],
                           priv_data->bit_ringbuffer_state_resource,
                           ((void*) (&buffer)))) {
        return mk_error(env, "bad_ringbuffer");
    }

    // Get the set bit
    unsigned int bit;
    if (!enif_get_uint(env, argv[1], &bit)) {
        return mk_error(env, "bad_value");
    }

    // Set/clear the bit
    if (bit) {
        buffer->bit_data[buffer->word_offset] |= (1 << buffer->word_index);
    } else {
        buffer->bit_data[buffer->word_offset] &= ~(1 << buffer->word_index);
    }

    // Increment the index on the buffer
    buffer->word_index++;
    if (buffer->word_index > 63) {
        buffer->word_index = 0;
        buffer->word_offset++;
    }

    // Wrap it if we ended up past the end of the buffer
    if (buffer->word_offset * 64 + buffer->word_index >= buffer->vector_size) {
        buffer->word_offset = 0;
        buffer->word_index = 0;
    }

    // All set
    return mk_atom(env, "ok");
}

static ERL_NIF_TERM erl_ringbuffer_popcnt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Assert we got one arg
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    // Grab the ringbuffer state
    struct PrivData *priv_data = enif_priv_data(env);
    struct bit_ringbuffer *buffer;
    if (!enif_get_resource(env, argv[0],
                           priv_data->bit_ringbuffer_state_resource,
                           ((void*) (&buffer)))) {
        return mk_error(env, "bad_ringbuffer");
    }

    // Popcnt the data
    uint64_t popcnt = popcnt_vector(buffer->bit_data, buffer->vector_size);

    // Return
    return enif_make_tuple2(env, mk_atom(env, "ok"), enif_make_uint64(env, popcnt));
}

static uint64_t popcnt_vector(uint64_t* vector, uint64_t vector_size_bits) {
    uint64_t vector_size_words = (vector_size_bits / 64) + (vector_size_bits % 64 == 0 ? 0 : 1);
    uint64_t popcnt = 0;
    for (uint64_t i = 0; i < vector_size_words; i++) {
        popcnt += __builtin_popcount(vector[i]);
    }
    return popcnt;
}

void destructor_bit_vector(ErlNifEnv* env, void* obj) {
    struct bit_vector *vector = obj;
    enif_release_resource(vector->bit_data);
}

void destructor_bit_ringbuffer(ErlNifEnv* env, void* obj) {
    struct bit_ringbuffer *buffer = obj;
    enif_release_resource(buffer->bit_data);
}

// Module callbacks
int load(ErlNifEnv* env, void** priv_data, UNUSED ERL_NIF_TERM load_info) {
    // Create our desired resource types
    ErlNifResourceFlags tried;
    ErlNifResourceType *bit_data_resource = enif_open_resource_type(
        env,
        NULL, // module_str (unused, must be NULL)
        "bit_data",
        NULL, // No destructor
        ERL_NIF_RT_CREATE,
        &tried
    );
    ErlNifResourceType *bit_vector_resource = enif_open_resource_type(
        env,
        NULL, // module_str (unused, must be NULL)
        "bit_vector_state",
        destructor_bit_vector,
        ERL_NIF_RT_CREATE,
        &tried
    );
    ErlNifResourceType *bit_ringbuffer_resource = enif_open_resource_type(
        env,
        NULL, // module_str (unused, must be NULL)
        "bit_ringbuffer_state",
        destructor_bit_ringbuffer,
        ERL_NIF_RT_CREATE,
        &tried
    );

    if (!bit_data_resource || !bit_vector_resource || !bit_ringbuffer_resource) {
        return 1;
    }

    // Store the resource type in our private data
    struct PrivData *data = malloc(sizeof(struct PrivData));
    data->bit_data_resource = bit_data_resource;
    data->bit_vector_state_resource = bit_vector_resource;
    data->bit_ringbuffer_state_resource = bit_ringbuffer_resource;

    // Update the priv_data with our PrivData struct
    *priv_data = data;

    // Return success
    return 0;
}

int upgrade(UNUSED ErlNifEnv* env, UNUSED void** priv_data,
            UNUSED void** old_priv_data, UNUSED ERL_NIF_TERM load_info) {
    // Nothing needs to be done when the module is reloaded
    return 0;
}

void unload(UNUSED ErlNifEnv* env, void* priv_data){
    // We need to free priv_data, which is a pointer to our PrivData struct
    free(priv_data);
}
