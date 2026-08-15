//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*

def Test[T](x: T) =
  x match
    case y: String? => println(y)       // warn
    case _ =>
  Ok(x) match
    case y: String? => println(y)       // warn
    case _ =>                           // warn
  x match
    case y: Option[String] => println(y)// warn
    case _ =>
  Some(x) match
    case y: Option[String] => println(y)// warn
    case _ =>                           // warn

