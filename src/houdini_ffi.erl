-module(houdini_ffi).
-export([coerce/1, slice/3, first/1, drop_first/1]).

coerce(X) -> X.

slice(String, From, Len) ->
        binary:part(String, From, Len).

first(String) ->
    case String of
        <<>> -> <<>>;
        <<First, _/bitstring>> ->
            <<First>>
    end.

drop_first(String) ->
    case String of
        <<>> -> <<>>;
        <<_, Rest/bitstring>> -> Rest
    end.
