// Compiled with 3.0.0 and run with current compiler
class Foo:
  lazy val x =
    println("computing x")
    "x"
  lazy val y =
    println("computing y")
    "y"

@main def Test =
  val foo = new Foo
  println(foo.x)
  println(foo.y)
