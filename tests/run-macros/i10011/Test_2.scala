
extension [T](a: T) def foo: Any = ???

@main def Test =
  val foo = printPos(Some(1).foo)
  assert(foo == (79, 90))
