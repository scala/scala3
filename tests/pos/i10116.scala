object OverloadedWithLong {
  def overloaded(x: Long): Any =
    x

  def overloaded(x: Any): Unit =
    ???
}

object Test {
  def main(args: Array[String]): Unit =
    import OverloadedWithLong.*

    val l: Any = 0 :: Nil
    val r = overloaded(l match {
      case x :: xs => 5
    })
    assert(r == 5L)
}