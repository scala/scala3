//> using options  -source future

def foo[T](x: T): Matchable =
  println(x.getClass())   // ok
  println(x.isInstanceOf[Int]) // ok
  x match
    case x: Int =>   // warn: should not be scrutinized
      println("int")
      x
    case x: String =>  // warn: should not be scrutinized
      println("string")
      x
  List(x) match
    case (x: Int) :: Nil =>  // warn: should not be scrutinized
      println("int")
      x
    case List(x: String) =>  // warn: should not be scrutinized
      println("string")
      x
    case List(y :: Nil) =>  // warn: should not be scrutinized
      y :: Nil
    case _ =>
      x   // warn: should not be scrutinized

@main def Test =
  val x: Matchable = foo(1)
  val y: Matchable = foo("hello")
  assert(x != y)