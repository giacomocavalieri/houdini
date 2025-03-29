-module(houdini_ffi).
-export([coerce/1, slice/3, drop_first/1]).

coerce(X) -> X.

slice(String, From, Len) ->
        binary:part(String, From, Len).

drop_first(String) ->
    case String of
        <<>> -> <<>>;
        <<_, Rest/bitstring>> -> Rest
    end.
