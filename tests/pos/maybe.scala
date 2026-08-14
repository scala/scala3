
import language.experimental.magic
import scala.magic.*

def toOptionStr(x: String?): Option[String] = x match
  case Ok(y) => Some(y)
  case null => None

def toOption[T](x: T?): Option[T] = x match
  case Ok(y) => Some(y)
  case null => None

def Test[T](x: T) =
  val x1 = toOption(Ok("s"))
  val x2 = toOption(null)

  val y1 = toOptionStr(Ok("s"))
  val y2 = toOptionStr(null)

  val z1 = toOption(Ok(x))
