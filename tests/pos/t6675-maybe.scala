//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import scala.util.{Ok, Err}

object LeftOrRight {
  def unapply[A](value: Either[A, A]): A? = value match {
    case scala.Left(x) => Ok(x)
    case scala.Right(x) => Ok(x)
  }
}

object Test {
  (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match {
    case LeftOrRight(pair @ (a, b)) => a // false -Wshadow warning: "extractor pattern binds a single value to a Product2 of type (Int, Int)"
  }

  (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match {
    case LeftOrRight((a, b)) => a // false -Wshadow warning: "extractor pattern binds a single value to a Product2 of type (Int, Int)"
  }

  (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match {
    case LeftOrRight(a, b) => a // false -Wshadow warning: "extractor pattern binds a single value to a Product2 of type (Int, Int)"
  }
}
