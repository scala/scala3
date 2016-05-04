object equality {

  trait Eq[-T, -U]
  object Eq extends Eq[Any, Any]

  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U)(implicit ce: Eq[T, U]) = x.equals(y)
  }

  type EqEq[T] = Eq[T, T]

  trait EqClass

  implicit def eqAny: Eq[Any, Any] = Eq

  implicit def mixedEq1   : Eq[Any, EqClass] = Eq
  implicit def mixedEq1alt: Eq[Any, EqClass] = Eq
  implicit def mixedEq2   : Eq[EqClass, Any] = Eq
  implicit def mixedEq2alt: Eq[EqClass, Any] = Eq

//  implicit def eqEq[U, T <: U with EqClass[U]]: Eq[T, T] = Eq

  case class Str(str: String) extends EqClass
  case class Num(x: Int) extends EqClass

  case class Other(x: Int)

  trait Option[+T] extends EqClass
  case class Some[+T](x: T) extends Option[T]
  case object None extends Option[Nothing]

  implicit def eqStr: Eq[Str, Str] = Eq
  implicit def eqNum: Eq[Num, Num] = Eq
  implicit def eqOption[T, U](implicit ce: Eq[T, U]): Eq[Option[T], Option[U]] = Eq

  def main(args: Array[String]): Unit = {
    val x = Str("abc")
    (Some(x): Option[Str]) === (None: Option[Str])

    x === x

    val n = Num(2)
    val m = Num(3)
    n === m

    Other(1) === Other(2)

    (Some(x) === None)//(eqOption)
    (Some(x) === Some(Str("")))(eqOption)
    val z: Option[Str] = Some(Str("abc"))
    z === Some(x)
    z === None
    Some(x) === z
    None === z
/*
    Other(3) === null
    Str("x") === null
    null === Other(3)
    null === Str("x")
    null === null*/

    class Fruit extends EqClass
    class Apple extends Fruit
    class Pear extends Fruit
    implicit def eqFruit: Eq[Fruit, Fruit] = Eq

    Some(new Apple) === Some(new Pear)


    def ddistinct[T: EqEq](xs: List[T]): List[T] = xs match {
      case Nil => Nil
      case x :: xs => x :: xs.filterNot(x === _)
    }

    ddistinct(List(z, z, z))

    n match {
      case None =>
    }

    Some(new Apple) === Some(Str("xx")) // error
    x === n  // error
    n === x  // error
    x === Other(1)  // error
    Other(2) === x // error
    z === Some(n) // error
    z === n // error
    Some(n) === z // error
    n === z // error
    Other(1) === z // error
    z === Other(1) // error
    ddistinct(List(z, n)) // error


  }
}
