@main def Test: Unit =
  println(macroPolyFun("foo", [Z] => (arg: Z) => arg.toString))
  println(macroFun("foo", arg => arg.toString))
