object Test {
  val xs = List(1, 2, 3)

  xs match {
    case x :: xs1 => println(x); println(xs1)
    case Nil => println("none")
  }

  xs.length match {
    case 0 => println("0")
    case 1: Int => println("1")
    case 2 => println("2")
    case 3 => println("3")
    case 4 => println("4")
    case _ => println("something else")
  }

  (xs.length, xs) match {
    case (0, Nil: List[Int]) => println("1")
    case (_, Nil) => println("2")
    case (0, _)   => println("3")
    case (x, y)   => println("4")
  }

  xs match {
    case 0 :: Nil => println("1")
    case _ :: Nil => println("2")
    case 0 :: _   => println("3")
    case x :: y   => println("4")
  }

  (xs.length, xs) match {
    case (_, Nil) | (0, _) => println("1 or 2")
    case (x, y)   => println("4")
  }

  enum Option[+T] {
    case Some[T](value: T) extends Option[T]
    case None
  }
  import Option.*

  val x: Option[String] = Some("abc")

  x match {
    case Some(s) => println(s)
    case None => println("nothing")
  }

  type IntPair = (Int, Int)
  ??? match {
    case (x, y): IntPair => x * y
  }
}
