

object boundary:

  final class Label[-T] extends caps.SharedCapability

  /** Abort current computation and instead return `value` as the value of
   *  the enclosing `boundary` call that created `label`.
   */
  def break[T](value: T)(using label: Label[T]): Nothing = ???

  def apply[T](body: Label[T] ?=> T): T = ???
end boundary

import boundary.{Label, break}

trait Async extends caps.SharedCapability
object Async:
  def blocking[T](body: Async ?=> T): T = ???

class Future[+T]:
  this: Future[T]^ =>
  def await(using Async): T = ???
object Future:
  def apply[T](op: Async ?=> T)(using Async): Future[T]^{op} = ???

enum Result[+T, +E]:
  case Ok[+T](value: T) extends Result[T, Nothing]
  case Err[+E](error: E) extends Result[Nothing, E]


object Result:
  extension [T, E](r: Result[T, E]^)(using Label[Err[E]])

    /** `_.ok` propagates Err to current Label */
    def ok: T = r match
      case Ok(value) => value
      case Err(value) => break[Err[E]](Err(value))

  transparent inline def apply[T, E](inline body: Label[Result[T, E]] ?=> T): Result[T, E] =
    boundary(Ok(body))

  // same as apply, but not an inline method
  def make[T, E](body: Label[Result[T, E]] ?=> T): Result[T, E] =
    boundary(Ok(body))

end Result

def test[T, E](using Async) =
  import Result.*
  Async.blocking: async ?=>
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
        Future: // error, type mismatch
          fr.await.ok

    def fail4[T, E](fr: Future[Result[T, E]]^) =
        Result.make:
          Future: fut ?=> // error, type mismatch
            fr.await.ok

    def fail5[T, E](fr: Future[Result[T, E]]^) =
        Result.make[Future[T], E]: lbl ?=>
          Future: fut ?=>  // error: type mismatch
            fr.await.ok

