def foo[T](x: T): Matchable =
  println(x.getClass())
  println(x.isInstanceOf[Int])
  x match
    case x: Int =>
      println("int")
      x
    case x: String =>
      println("string")
      x
  List(x) match
    case (x: Int) :: Nil =>
      println("int")
      x
    case List(x: String) =>
      println("string")
      x
    case List(y :: Nil) =>
      y :: Nil
    case _ =>
      g(x)

def g[T <: Matchable](x: T) = x

@main def Test =
  val x: Matchable = foo(110000)
  val y: Matchable = foo("hello")
  assert(x != y)
