object equality {

  trait Eql[T, U]
  def Eql[T, U]: Eql[T, U] = new Eql[T, U]{}

  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U)(implicit ce: Eql[T, U]) = x.equals(y)
  }

  type EqEq[T] = Eql.derived[T, T]

  trait EqClass[T]

  implicit def eqAny[T, U]: Eql[T, U] = Eql.derived
/*
  implicit def mixedEq1[T, U](implicit ce: Eql[T, U]): Eql[T, Any] = Eql.derived
  implicit def mixedEq2[T, U](implicit ce: Eql[T, U]): Eql[Any, T] = Eql.derived
  implicit def mixedEq1alt : Eql[Any, EqClass] = Eql.derived
  implicit def mixedEq2    : Eql[EqClass, Any] = Eql.derived
  implicit def mixedEq2alt : Eql[EqClass, Any] = Eql.derived
  implicit def mixedNull1[T]: Eql[T, Null]     = Eql
  implicit def mixedNull2[T]: Eql[Null, T]     = Eql
*/
  implicit def eqString: Eql[String, String] = Eql
  implicit def eqInt: Eql[Int, Int] = Eql.derived
  implicit def eqOption[T, U](implicit ce: Eql[T, U]): Eql[Option[T], Option[U]] = Eql.derived

/*
  implicit def eqEq[UE, TE <: UE with EqClass[UE]]: Eql[TE, TE] = Eql

  case class Str(str: String) extends EqClass[Str]
  case class Num(x: Int) extends EqClass[Num]

  case class Other(x: Int)

  trait Option[+T] extends EqClass[_]
  case class Some[+T](x: T) extends Option[T]
  case object None extends Option[Nothing]

  //implicit def eqStr: Eql[Str, Str] = Eql.derived
   //implicit def eqNum: Eql[Num, Num] = Eql.derived

  implicit def eqOption[T, U](implicit ce: Eql[T, U]): Eql[Option[T], Option[U]] = Eql
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
    implicit def eqFruit: Eql[Fruit, Fruit] = Eql

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
