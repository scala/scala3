

def summonsTest = 
  given Type[String] = ???
  val opt1: Option[Wrapper[String]] = Wrapper.unapply[String] // ok
  val opt2 = Wrapper.unapply[String]
  opt2.map(_.getValue) // only error when using workaround

def patternMatchTest =
  Type[String] match 
    case Wrapper(v) => v.getValue // was error // was error

type Type[A] = Class[A] // any rhs would work here
object Type:
  def apply[A]: Type[A] = ???

trait Wrapper[T]:
  def getValue: T = ???
object Wrapper:
  def unapply[T](implicit ev: Type[T]): Option[Wrapper[T]] = None
    
  /* Workaround: 
  @annotation.targetName("unapplyValue")
  def unapply[T](ev: Type[T]): Option[Wrapper[T]] = unapply(using ev)
  
  @annotation.targetName("unapplySummon")
  def unapply[T](using Type[T]): Option[Wrapper[T]] = ???
  */
