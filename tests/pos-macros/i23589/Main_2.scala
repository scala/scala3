//> using options -experimental
@main def main() =
  testMacro([B] => (a : B, b : B) => (a, b))
