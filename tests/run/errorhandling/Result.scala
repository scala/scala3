package scala.util
import boundary.Label

abstract class Result[+T, +E]
case class Ok[+T](value: T)  extends Result[T, Nothing]
case class Err[+E](value: E) extends Result[Nothing, E]

object Result:
  extension [T, E](r: Result[T, E])

    /** `_.?` propagates Err to current Label */
    transparent inline def ? (using Label[Err[E]]): T = r match
      case r: Ok[_] => r.value
      case err => break(err.asInstanceOf[Err[E]])

    /** If this is an `Err`, map its value */
    def mapErr[E1](f: E => E1): Result[T, E1] = r match
      case err: Err[_] => Err(f(err.value))
      case ok: Ok[_] => ok

    /** Map Ok values, propagate Errs */
    def map[U](f: T => U): Result[U, E] = r match
      case Ok(x) => Ok(f(x))
      case err: Err[_] => err

    /** Flatmap Ok values, propagate Errs */
    def flatMap[U](f: T => Result[U, E]): Result[U, E] = r match
      case Ok(x) => f(x)
      case err: Err[_] => err

  /** Similar to `Try`: Convert exceptions raised by `body` to `Err`s.
   */
  def apply[T](body: => T): Result[T, Exception] =
    try Ok(body)
    catch case ex: Exception => Err(ex)
end Result

/** A prompt for `_.?`. It establishes a boundary to which `_.?` returns */
object respond:
  inline def apply[T, E](inline body: Label[Err[E]] ?=> T): Result[T, E] =
    boundary:
      val result = body
      Ok(result)


