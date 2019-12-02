case class Foo(x: Int, y: String)

val x = Foo(1, "2")
val y: (Int, Boolean) = Tuple.fromProductTyped(x)  // error

case class Bar[I <: Int, S <: String](x: I, y: S)
val a: Bar[1, "2"] = Bar(1, "2")
val b: (1, "3") = Tuple.fromProductTyped(a)  // error
