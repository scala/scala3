//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*
import scala.util.Either

def toOptionMissingNull[T](x: T?): Option[T] = x match // warn
  case Ok(y) => Some(y)

def toOptionMissingOk[T](x: T?): Option[T] = x match // warn
  case null => None

def toEitherMissingErr(x: Int ? String): Either[String, Int] = x match // warn
  case Ok(y) => Right(y)

def toEitherMissingOk(x: Int ? String): Either[String, Int] = x match // warn
  case Err(e) => Left(e)
