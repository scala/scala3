def foo(x: AnyRef | Null): Unit =
  println(x)

@main def main(): Unit =
  foo(10) // error
