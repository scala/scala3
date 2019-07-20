@main def Test =
  println("Hello, world!")

object A {
  @main def foo(x: Int, y: String, zs: Float*) =
    println(s"I found: $x, $y, $zs")
}
