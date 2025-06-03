trait T:
  given x: Int
  given y(using Int): String
  given z[T](using T): Seq[T]

object Test extends T, App:
  given x: Int = 22
  override given y(using Int): String = summon[Int].toString
  given z: [T] => T => Seq[T]:
    override def apply(x: Int) = ???
    override def length = ???
    override def iterator = ???
    override def toString = s"seq $x"

  assert(summon[Int] == 22)
  assert(summon[String] == "22")
  assert(summon[Seq[Int]].toString == "seq 22")


