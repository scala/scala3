trait T {
  object O

  type U = (given Int) => Int
  type V = (given Int) => Int

  val u = delegate (x: Int) => x
  val v = given (x: Int) => x
  val w = (given x: Int) => x
  val w2: V = (given x) => x

  class C[T]

  given t2[T](given C[T]): C[T]

  try ??? : List[Int] match
    case Nil => 1
    case x :: xs => 2
  catch
    case java.io.IOException => 3
    case java.lang.Error => 4
}
