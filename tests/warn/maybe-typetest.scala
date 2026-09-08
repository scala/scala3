//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import language.future
import scala.util.{Ok, Err}

def Test[T](x: T) =
  x match
    case y: String? => println(y)       // warn typetest // warn unmatchable
    case _ =>
  Ok(x) match
    case y: String? => println(y)       // warn typetest?
    case _ =>                           // warn unreachable
  x match
    case y: Option[String] => println(y)// warn typetest // warn unmatchable
    case _ =>
  Some(x) match
    case y: Option[String] => println(y)// warn typetest
    case _ =>                           // warn unreachable

