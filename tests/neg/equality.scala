
object equality {

  case class Str(str: String) extends EqClass

  case class Num(x: Int) extends EqClass[Num]

  case class Other(x: Int)

  trait Option[+T] extends EqClass
  case class Some[+T](x: T) extends Option[T]
  case object None extends Option[Nothing]

  implicit def eqStr: Eq[Str] = Eq
  implicit def eqOption[T: Eq]: Eq[Option[T]] = Eq

  class PreName extends EqClass
  implicit def eqPreName: Eq[PreName] = Eq

  implicit def fromString(str: String): PreName = ???
  val name = "abc"
  name == "def"



  def main(args: Array[String]): Unit = {
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

    Other(3) == null
    Str("x") == null
    null == Other(3)
    null == Str("x")
    null == null

    class Fruit extends EqClass

    implicit def eqFruit: Eq[Fruit] = Eq
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

    def ddistinct[T: Eq](xs: List[T]): List[T] = xs match {
      case Nil => Nil
      case x :: xs => x :: xs.filterNot(x == _)
    }

    ddistinct(List(z, z, z))

    n match {
      case None =>   // error
    }

    Some(new Apple) == Some(Str("xx")) // error
    x == n  // error
    n == x  // error
    z == Some(n) // error
    z == n // error
    Some(n) == z // error
    n == z // error
    ddistinct(List(z, n)) // error
  }
}
