import eu.joaocosta.defaultstest._

@main def Test =
  println(Foo.foo(2)) // Works
  println(Bar.foo(2)) // Fails with "missing argument for parameter y of method foo in object Bar: (x: Int, y: Int): Int"
