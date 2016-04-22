package strawman.equality
import annotation.implicitNotFound

object EqualityStrawman1 {

  trait Eq[-T]

  @implicitNotFound("cannot compare value of type ${T} with a value outside its equality class")
  trait Impossible[T]

  object Eq extends Eq[Any]

  trait Base {
    def === (other: Any): Boolean = this.equals(other)
    def === (other: Null): Boolean = this.equals(other)
    def === [T <: EqClass](other: T)(implicit ce: Impossible[T]): Boolean = ???
  }

  trait EqClass[-U] extends Base {
    def === [T >: this.type <: EqClass](other: T)(implicit ce: Eq[T]): Boolean = this.equals(other)
    def === [T](other: T)(implicit ce: Impossible[T]): Boolean = ???
    def === (other: Null): Boolean = this.equals(other)
  }

  case class Str(str: String) extends EqClass[_]

  case class Num(x: Int) extends EqClass[Num]

  case class Other(x: Int) extends Base

  trait Option[+T] extends EqClass[_]
  case class Some[+T](x: T) extends Option[T]
  case object None extends Option[Nothing]

  implicit def eqStr: Eq[Str] = Eq
  //implicit def eqNum: Eq[Num] = Eq
  implicit def eqOption[T: Eq]: Eq[Option[T]] = Eq

  implicit def eqEq[T <: EqClass[T]]: Eq[T] = Eq

  def main(args: Array[String]): Unit = {
    val x = Str("abc")
    x === x

    val n = Num(2)
    val m = Num(3)
    n === m

    Other(1) === Other(2)

    Some(x) === None
    Some(x) === Some(Str(""))
    val z: Option[Str] = Some(Str("abc"))
    z === Some(x)
    z === None
    Some(x) === z
    None === z

    Other(3) === null
    Str("x") === null

    def ddistinct[T <: Base: Eq](xs: List[T]): List[T] = xs match {
      case Nil => Nil
      case x :: xs => x :: xs.filterNot(x === _)
    }

    ddistinct(List(z, z, z))

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
