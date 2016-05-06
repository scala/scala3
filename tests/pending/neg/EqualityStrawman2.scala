object equality {

  trait Eq[T, U]
  def Eq[T, U]: Eq[T, U] = new Eq[T, U]{}

  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U)(implicit ce: Eq[T, U]) = x.equals(y)
  }

  type EqEq[T] = Eq[T, T]

  trait EqClass[T]

  implicit def eqAny[T, U]: Eq[T, U] = Eq
/*
  implicit def mixedEq1[T, U](implicit ce: Eq[T, U]): Eq[T, Any] = Eq
  implicit def mixedEq2[T, U](implicit ce: Eq[T, U]): Eq[Any, T] = Eq
  implicit def mixedEq1alt : Eq[Any, EqClass] = Eq
  implicit def mixedEq2    : Eq[EqClass, Any] = Eq
  implicit def mixedEq2alt : Eq[EqClass, Any] = Eq
  implicit def mixedNull1[T]: Eq[T, Null]     = Eq
  implicit def mixedNull2[T]: Eq[Null, T]     = Eq
*/
  implicit def eqString: Eq[String, String] = Eq
  implicit def eqInt: Eq[Int, Int] = Eq
  implicit def eqOption[T, U](implicit ce: Eq[T, U]): Eq[Option[T], Option[U]] = Eq

/*
  implicit def eqEq[UE, TE <: UE with EqClass[UE]]: Eq[TE, TE] = Eq

  case class Str(str: String) extends EqClass[Str]
  case class Num(x: Int) extends EqClass[Num]

  case class Other(x: Int)

  trait Option[+T] extends EqClass[_]
  case class Some[+T](x: T) extends Option[T]
  case object None extends Option[Nothing]

  //implicit def eqStr: Eq[Str, Str] = Eq
   //implicit def eqNum: Eq[Num, Num] = Eq

  implicit def eqOption[T, U](implicit ce: Eq[T, U]): Eq[Option[T], Option[U]] = Eq
*/
  def some[T](x: T): Option[T] = Some(x)

  def main(args: Array[String]): Unit = {
    val x = "abc"
    x === x

    val n = 2
    n === n
    x === 1 // error
    n === x // error

    1.0 === 1.0
    1.0 === new Object
    1.0 === n // error

    some(n) === some(n)
    some(n) === 1 // error
    some(n) === some(1) // error
    1 === some(n) // error
    some(1) === some(n)
    some(1.0) === some(new Object)
    some(1) === some(x) // error
    some(x) === some(1) // error
    some(x) === some(1.0)

/*
    val n = Num(2)
    val m = Num(3)
    n === m

    Other(1) === Other(2)

    Some(x) === Some(Str(""))
    val z: Option[Str] = Some(Str("abc"))
    z === Some(x)
    z === None
    Some(x) === z
    None === z
    Some(Other(1)) === None

    Other(3) === null
    Str("x") === null
//    null === Other(3)
//    null === Str("x")
//    null === null

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
    Some(new Apple) === Some(Other(1)) // error
    Some(Other(1)) === Some(new Apple) // error
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
*/
  }
}
