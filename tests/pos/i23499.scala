def summonsTest =
  given Type[String] = ???
  val opt1: Option[Wrapper[String]] = Wrapper.unapply[String] // ok
  val opt2 = Wrapper.unapply[String]
  opt2.map(_.getValue)

def patternMatchTest =
  Type[String] match
    case Wrapper(v) => v.getValue // was an error

type Type[A] = Class[A] // any rhs would work here
object Type:
  def apply[A]: Type[A] = ???

trait Wrapper[T]:
  def getValue: T = ???
object Wrapper:
  def unapply[T](implicit ev: Type[T]): Option[Wrapper[T]] = None
