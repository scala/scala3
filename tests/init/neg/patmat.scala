object A:                             // error
  val a: Option[Int] = Some(3)
  a match
  case Some(x) => println(x * 2 + B.a.size)
  case None => println(0)

object B:
  val a = 3 :: 4 :: Nil
  a match
  case x :: xs =>
    println(x * 2)
    if A.a.isEmpty then println(xs.size)
  case Nil =>
    println(0)
