def foo[T](x: T): Matchable =
  println(x.getClass())   // ok
  println(x.isInstanceOf[Int]) // ok
  x match
    case x: Int =>   // error: should not be scrutinized
      println("int")
      x
    case x: String =>  // error: should not be scrutinized
      println("string")
      x
  List(x) match
    case (x: Int) :: Nil =>  // error: should not be scrutinized
      println("int")
      x
    case List(x: String) =>  // error: should not be scrutinized
      println("string")
      x
    case List(y :: Nil) =>  // error: should not be scrutinized
      y :: Nil
    case _ =>
      x   // error: should not be scrutinized

@main def Test =
  val x: Matchable = foo(1)
  val y: Matchable = foo("hello")
  assert(x != y)

