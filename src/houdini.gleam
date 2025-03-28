@target(javascript)
import internals/escape_generic as escape

@target(erlang)
import internals/escape_erl as escape

/// Escapes a string in a format suitable to be used inside HTML by escaping
/// the following characters: `<`, `>`, `&`, `"`, `'`.
///
/// ## Examples
///
/// ```gleam
/// assert escape("wibble & wobble") == "wibble &amp; wobble";
/// assert escape("wibble > wobble") == "wibble &gt; wobble";
/// ```
///
pub fn escape(string: String) -> String {
  escape.escape(string)
}
