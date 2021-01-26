trait A
trait B
class C extends A, B
object A:
  def unapply(x: A): Option[String] = Some(x.toString)
  def unapply(x: B): Option[String] = Some(x.toString)

object B

object D:
  def unapply(x: A, y: B): Option[String] = Some(x.toString)

object E:
  val unapply: Option[String] = Some("")

object F:
  def unapply(x: Int): Boolean = true


@main def Test =
  C() match
    case A("2") => // error (cannot resolve overloading)
    case B("2") => // error (cannot be used as an extractor)
    case D("2") => // error (cannot be used as an extractor)
    case E("2") => // error (value unapply in object E does not take parameters)
    case F("2") => // error (Wrong number of argument patterns for F; expected: ())
    case G("2") => // error (Not found: G)
end Test
