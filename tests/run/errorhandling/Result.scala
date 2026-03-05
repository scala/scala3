package scala.util
import boundary.{Label, break}

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

    /** Replace OK values, propagate Errs */
    def ifOK[U](u: => U): Result[U, E] = r match
      case ok: Ok[_] => Ok(u)
      case err: Err[_] => err

    /** Map Ok values, propagate Errs */
    def map[U](f: T => U): Result[U, E] = r match
      case Ok(x) => Ok(f(x))
      case err: Err[_] => err

    /** Flatmap Ok values, propagate Errs */
    def flatMap[U](f: T => Result[U, E]): Result[U, E] = r match
      case Ok(x) => f(x)
      case err: Err[_] => err

    /** Validate both `r` and `other`; return a pair of successes or a list of failures. */
    def zip[U](other: Result[U, E]): Result[(T, U), List[E]] = (r, other) match
      case (Ok(x), Ok(y))     => Ok((x, y))
      case (Ok(_), Err(e))    => Err(e :: Nil)
      case (Err(e), Ok(_))    => Err(e :: Nil)
      case (Err(e1), Err(e2)) => Err(e1 :: e2 :: Nil)

    /** Validate both `r` and `other`; return a tuple of successes or a list of failures.
     *  Unlike with `zip`, the right hand side `other` must be a `Result` returning a `Tuple`,
     *  and the left hand side is added to it. See `Result.empty` for a convenient
     *  right unit of chains of `*:`s.
     */
    def *: [U <: Tuple](other: Result[U, List[E]]): Result[T *: U, List[E]] = (r, other) match
      case (Ok(x), Ok(ys))     => Ok(x *: ys)
      case (Ok(_), es: Err[?]) => es
      case (Err(e), Ok(_))     => Err(e :: Nil)
      case (Err(e), Err(es))   => Err(e :: es)
  end extension

  /** Similar to `Try`: Convert exceptions raised by `body` to `Err`s.
   */
  def apply[T](body: => T): Result[T, Exception] =
    try Ok(body)
    catch case ex: Exception => Err(ex)

  /** Right unit for chains of `*:`s. Returns an `Ok` with an `EmotyTuple` value. */
  def empty: Result[EmptyTuple, Nothing] = Ok(EmptyTuple)
end Result

/** A prompt for `_.?`. It establishes a boundary to which `_.?` returns */
object respond:
  inline def apply[T, E](inline body: Label[Err[E]] ?=> T): Result[T, E] =
    boundary:
      val result = body
      Ok(result)


