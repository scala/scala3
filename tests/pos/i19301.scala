//> using options -Xfatal-warnings

object Extensions:
  infix def foo(x: String): Unit = ()
  extension (arg1: Int) infix def X (arg2: Int): Int = arg1 * arg2
  infix type X[A, B]

export Extensions.*

val x = 1 X 2
type Foo = Int X Int
val u = Extensions foo ""
