import annotation.capability

object boundary:

  @capability final class Label[-T]

  /** Abort current computation and instead return `value` as the value of
   *  the enclosing `boundary` call that created `label`.
   */
  def break[T](value: T)(using label: Label[T]): Nothing = ???

  def apply[T](body: Label[T] ?=> T): T = ???
end boundary

import boundary.{Label, break}

class Async
class Future[+T]:
  this: Future[T]^ =>
  def await(using Async^): T = ???
object Future:
  def apply[T](op: Async^ ?=> T)(using Async): Future[T]^{op} = ???

abstract class Result[+T, +E]
case class Ok[+T](value: T)  extends Result[T, Nothing]
case class Err[+E](value: E) extends Result[Nothing, E]

object Result:
  extension [T, E](r: Result[T, E])

    /** `_.ok` propagates Err to current Label */
    inline def ok(using Label[Result[Nothing, E]]): T = r match
      case r: Ok[_] => r.value
      case err => break(err.asInstanceOf[Err[E]])

  transparent inline def apply[T, E](inline body: Label[Result[T, E]] ?=> T): Result[T, E] =
    boundary:
      val result = body
      Ok(result)

  // same as apply, but not an inline method
  def make[T, E](body: Label[Result[T, E]] ?=> T): Result[T, E] =
    boundary:
      val result = body
      Ok(result)

end Result

def test[T, E](using Async) =
  val good1: List[Future[Result[T, E]]] => Future[Result[List[T], E]] = frs =>
    Future:
      Result:
        frs.map(_.await.ok) // OK

  val good2: Result[Future[T], E] => Future[Result[T, E]] = rf =>
    Future:
      Result:
        rf.ok.await  // OK, Future argument has type Result[T]

  def fail3(fr: Future[Result[T, E]]^) =
    Result:
      Future: // error, escaping label from Result
        fr.await.ok

  def fail4(fr: Future[Result[T, E]]^) =
    Result.make: // error, escaping label from Result
      Future:
        fr.await.ok
