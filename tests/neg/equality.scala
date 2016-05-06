object equality {

  case class Str(str: String)

  case class Num(x: Int)

  case class Other(x: Int)

  trait Option[+T]
  case class Some[+T](x: T) extends Option[T]
  case object None extends Option[Nothing]

  implicit def eqStr: Eq[Str, Str] = Eq()
  implicit def eqNum: Eq[Num, Num] = Eq()
  implicit def eqOption[T, U, OT <: Option[T], OU <: Option[U]]
    (implicit e: Eq[T, U]): Eq[OT, OU] = Eq()

  def main(args: Array[String]): Unit = {
    Some(Other(3)) == None

    val x = Str("abc")
    x == x

    val n = Num(2)
    val m = Num(3)
    n == m

    Other(1) == Other(2)

    Some(x) == None
    Some(x) == Some(Str(""))
    val z: Option[Str] = Some(Str("abc"))
    z == Some(x)
    z == None
    Some(x) == z
    None == z
    None == Some(Other(3))

    Other(3) == null
    Str("x") == null
    null == Other(3)
    null == Str("x")
    null == null

    class Fruit

    implicit def eqFruit[A <: Fruit, B <: Fruit]: Eq[A, B] = Eq()

    class Apple extends Fruit
    class Pear extends Fruit
    val a = new Apple
    val p = new Pear
    val f: Fruit = a
    a == p
    p == a
    f == p
    p == f
    Some(new Apple) == Some(new Pear)

    n match {
      case None =>   // error
    }

    x == Other(1)  // error
    Other(2) == x // error
    Other(1) == z // error
    z == Other(1) // error
    n == None // error

    Some(new Apple) == Some(Str("xx")) // error
    x == n  // error
    n == x  // error
    z == Some(n) // error
    z == n // error
    Some(n) == z // error
    Some(n) == Some(Other(3)) // error
    Some(Other(3)) == Some(n) // error
    n == z // error
  }
}
