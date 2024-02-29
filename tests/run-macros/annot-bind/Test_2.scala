//> using options -experimental -Yno-experimental

@bind("a")
val foo: String = "foo"

@bind("a") @bind("b")
val bar: String = "bar"

@main def Test =
  assert(foo == "foo")
  assert(bar == "bar")
