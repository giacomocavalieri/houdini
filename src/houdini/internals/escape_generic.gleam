import gleam/list
import gleam/string

/// This `escape` function will work on all targets, beware that the version
/// specifically optimised for Erlang will be _way faster_ than this one when
/// running on the BEAM. That's why this fallback implementation is only ever
/// used when running on the JS backend.
///
pub fn escape(text: String) -> String {
  do_escape(text, 0, text, [], 0, False)
  |> list.reverse
  |> string.join(with: "")
}

// The logic behind this function is exactly the same as the erlang one: we
// iterate the string byte by byte and only ever take slices of it (constant
// time operation that ensures maximum sharing). However, this implementation is
// a little more convoluted since we cannot define it as two mutually recursive
// functions as we did with the Erlang one (or it won't be tail call optimised
// on the JS target).
fn do_escape(
  string: String,
  skip: Int,
  original: String,
  acc: List(String),
  len: Int,
  found_normal: Bool,
) -> List(String) {
  case found_normal, first(string) {
    False, "<" -> {
      let rest = drop_first(string)
      let acc = ["&lt;", ..acc]
      do_escape(rest, skip + 1, original, acc, 0, False)
    }

    False, ">" -> {
      let rest = drop_first(string)
      let acc = ["&gt;", ..acc]
      do_escape(rest, skip + 1, original, acc, 0, False)
    }

    False, "&" -> {
      let rest = drop_first(string)
      let acc = ["&amp;", ..acc]
      do_escape(rest, skip + 1, original, acc, 0, False)
    }

    False, "\"" -> {
      let rest = drop_first(string)
      let acc = ["&quot;", ..acc]
      do_escape(rest, skip + 1, original, acc, 0, False)
    }

    False, "'" -> {
      let rest = drop_first(string)
      let acc = ["&#39;", ..acc]
      do_escape(rest, skip + 1, original, acc, 0, False)
    }

    False, "" -> acc

    // For any other bit that doesn't need to be escaped we go into an inner
    // loop, consuming as much "non-escapable" chars as possible.
    False, _ -> {
      let rest = drop_first(string)
      do_escape(rest, skip, original, acc, 1, True)
    }

    True, "<" -> {
      let rest = drop_first(string)
      let slice = slice(original, skip, len)
      let acc = ["&lt;", slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc, 0, False)
    }

    True, ">" -> {
      let rest = drop_first(string)
      let slice = slice(original, skip, len)
      let acc = ["&gt;", slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc, 0, False)
    }

    True, "&" -> {
      let rest = drop_first(string)
      let slice = slice(original, skip, len)
      let acc = ["&amp;", slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc, 0, False)
    }

    True, "\"" -> {
      let rest = drop_first(string)
      let slice = slice(original, skip, len)
      let acc = ["&quot;", slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc, 0, False)
    }

    True, "'" -> {
      let rest = drop_first(string)
      let slice = slice(original, skip, len)
      let acc = ["&#39;", slice, ..acc]
      do_escape(rest, skip + len + 1, original, acc, 0, False)
    }

    True, "" ->
      case skip {
        0 -> [original]
        _ -> {
          let slice = slice(original, skip, len)
          [slice, ..acc]
        }
      }

    // If a char doesn't need escaping we keep increasing the length of the
    // slice we're going to take.
    True, _ -> {
      let rest = drop_first(string)
      do_escape(rest, skip, original, acc, len + 1, True)
    }
  }
}

@external(erlang, "houdini_ffi", "first")
@external(javascript, "../../houdini.ffi.mjs", "first")
fn first(string: String) -> String

@external(erlang, "houdini_ffi", "drop_first")
@external(javascript, "../../houdini.ffi.mjs", "drop_first")
fn drop_first(string: String) -> String

@external(erlang, "houdini_ffi", "slice")
@external(javascript, "../../houdini.ffi.mjs", "slice")
fn slice(string: String, from: Int, to: Int) -> String
