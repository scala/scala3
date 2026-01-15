trait <[+A, -S]
object `<`:
  extension [A, S](v: A < S) def map[B, S2](f: A => B < S2): B < (S & S2) = ???

def lift[A, S](v: A): A < S = ???

abstract class ArrowEffect[-Input[_], +Output[_]]
object ArrowEffect:
  inline def handleCatching[I[_], O[_], E <: ArrowEffect[I, O], A, B, S, S2, S3](
      v: A < (E & S)
  ): B < (S & S2 & S3) = ???

trait Result[+E, +A]
object Result:
  def succeed[E, A](value: A): Result[E, A] = ???

abstract class Error[+E]

type Const[A] = [B] =>> A
sealed trait Abort[-E] extends ArrowEffect[Const[Error[E]], Const[Unit]]
object Abort:
  inline def runWith[E, A, S, ER, B, S2](v: A < (Abort[E | ER] & S)) =
    ArrowEffect.handleCatching[
      Const[Error[E]],
      Const[Unit],
      Abort[E],
      Result[E, A],
      B,
      Abort[ER] & S,
      Abort[ER] & S,
      S2
    ](
      v.map: 
        value => lift:
          Result.succeed[E, A](value)
    )
