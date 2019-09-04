case class Foo(x: Int, y: String)

val x = Foo(1, "2")
val y: (Int, String) = Tuple.fromProductTyped(x)

case class Bar[I <: Int, S <: String](x: I, y: S)
val a: Bar[1, "2"] = Bar(1, "2")
val b: (1, "2") = Tuple.fromProductTyped(a)
