trait T:
  given x: Int
  given y(using Int): String = summon[Int].toString
  given z[T](using T): List[T]

object Test extends T:
  given x: Int = 22
  given y(using Int): String = summon[Int].toString * 22 // error
  given z[T](using T): Seq[T] = List(summon[T]) // error

  given s: [T] => T => Seq[T]: // error
    def apply(x: Int) = ???
    override def length = ???

end Test

