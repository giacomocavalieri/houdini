import gleam/list
import gleam/string
import gleeunit
import houdini
import qcheck.{type Generator}

pub fn main() -> Nil {
  gleeunit.main()
}

const conversions = [
  #("&", "&amp;"),
  #("<", "&lt;"),
  #(">", "&gt;"),
  #("\"", "&quot;"),
  #("'", "&#39;"),
]

pub fn simple_conversions_test() {
  use #(value, escaped) <- list.each(conversions)
  assert houdini.escape(value) == escaped
}

pub fn strange_unicode_string_test() {
  let input = ">a>'ࣉa>aa<a'>><\"aa&aࣉ>aࣉaaaa>ࣉa\"a'&a<<<&\"aaa\"&a>aa\">><'ࣉ\""
  assert houdini.escape(input) == escaped(input)
}

pub fn regular_string_is_left_unchanged_test() {
  let codepoints =
    qcheck.from_generators(qcheck.alphanumeric_ascii_codepoint(), [
      qcheck.ascii_whitespace_codepoint(),
    ])

  use regular_string <- given(qcheck.string_from(codepoints))
  assert houdini.escape(regular_string) == regular_string
}

pub fn string_with_special_characters_is_escaped_test() {
  let codepoints =
    qcheck.from_generators(escaped_codepoints(), [
      qcheck.alphanumeric_ascii_codepoint(),
    ])

  use string <- given(qcheck.string_from(codepoints))
  assert houdini.escape(string) == escaped(string)
}

// --- PROPERTY HELPERS --------------------------------------------------------

fn escaped_codepoints() -> Generator(UtfCodepoint) {
  let assert [first, ..rest] = {
    use #(value, _) <- list.map(conversions)
    let assert [codepoint] = string.to_utf_codepoints(value)
    qcheck.constant(codepoint)
  }
  qcheck.from_generators(first, rest)
}

fn given(generator: Generator(a), assertion: fn(a) -> Nil) -> Nil {
  let config = qcheck.default_config() |> qcheck.with_test_count(5000)
  qcheck.run(config, generator, assertion)
}

fn escaped(string: String) -> String {
  use string, #(value, escaped) <- list.fold(over: conversions, from: string)
  string.replace(in: string, each: value, with: escaped)
}
