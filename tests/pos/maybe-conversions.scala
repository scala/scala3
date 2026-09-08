//> using options -Yexplicit-nulls
import language.experimental.errorHandling

import scala.util.{Either, Ok, Err}

def toOptionAny[T](x: Any): Option[Any] = x match
  case Ok(y) => Some(y)
  case null => None

def toOption[T](x: T?): Option[T] = x match
  case Ok(y) => Some(y)
  case null => None

def toOptionStr(x: String?): Option[String] = x match
  case Ok(y) => Some(y)
  case null => None

def toEither[T, E](x: T ? E): Either[E, T] = x match
  case Ok(y) => Right(y)
  case Err(e) => Left(e)

def toEitherIntStr[T, E](x: Int ? String): Either[String, Int] = x match
  case Ok(y) => Right(y)
  case Err(e) => Left(e)
