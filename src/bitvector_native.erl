-module(bitvector_native).
-export([
    vector_new/1,
    vector_get/2,
    vector_set/3,
    ringbuffer_new/1,
    ringbuffer_append/2,
    ringbuffer_popcnt/1
]).
-on_load(init/0).

-define(LIB_NAME, bitvector).

vector_new(_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

vector_get(_, _) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

vector_set(_, _, _) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

ringbuffer_new(_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

ringbuffer_append(_, _) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

ringbuffer_popcnt(_) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]}).

init() ->
    SoName = case code:priv_dir(?LIB_NAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIB_NAME]);
                _ ->
                    filename:join([priv, ?LIB_NAME])
            end;
        Dir ->
            filename:join(Dir, ?LIB_NAME)
    end,
    erlang:load_nif(SoName, 0).
