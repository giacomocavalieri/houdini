import gleam/int
import gleam/io
import gleam/list
import gleam/string
import gleamy/bench
import houdini
import simplifile

pub fn main() {
  let html = read_project_files()
  let bytes = string.byte_size(html)
  let no_escapes = string.repeat("a", bytes)
  let size = int.to_string(bytes / 1024) <> "KB"

  bench.run(
    [
      bench.Input("just_html  (" <> size <> ")", html),
      bench.Input("no_escapes (" <> size <> ")", no_escapes),
    ],
    [bench.Function("houdini.escape", houdini.escape)],
    [bench.Duration(5000), bench.Warmup(2000)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.P(99)])
  |> io.println
}

fn read_project_files() -> String {
  let assert Ok(files) = simplifile.get_files(".")
  use acc, file <- list.fold(over: files, from: "")
  case string.ends_with(file, ".html") {
    False -> acc
    True -> {
      let assert Ok(file) = simplifile.read(file)
      acc <> file
    }
  }
}
