import language.experimental.errorHandling
import language.future

import scala.util.{Either, Ok, Err}

def toEither[T, E](x: T ? E): Either[E, T] = x match
  case Ok(y) => Right(y)
  case Err(e) => Left(e)


object Extract:
  def unapply[T](x: T): T ? String = Ok(x)

@main def Test = 22 match
  case Extract(s) =>
    if (true) println(s)
