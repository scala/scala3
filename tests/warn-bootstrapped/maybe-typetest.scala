//> using options -Yexplicit-nulls -Werror
import language.experimental.magic
import scala.magic.*

def Test[T](x: T) =
  x match
    case y: String? => println(y)       // error
    case _ =>
  Ok(x) match
    case y: String? => println(y)       // error
    case _ =>                           // error
  x match
    case y: Option[String] => println(y)// error
    case _ =>
  Some(x) match
    case y: Option[String] => println(y)// error
    case _ =>                           // error

