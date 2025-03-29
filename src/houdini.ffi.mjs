export function slice(string, from, len) {
  return string.slice(from, from + len);
}

export function drop_first(string) {
  return string.slice(1);
}
