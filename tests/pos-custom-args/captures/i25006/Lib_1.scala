package ccbug

import language.experimental.captureChecking
import caps.Control
import scala.annotation.unchecked.uncheckedCaptures

trait Cap extends Control

trait Monad[F[_]]:
  type Ctx <: Cap @uncheckedCaptures
  def pure[T](t: T): F[T]

object Monad:
  type Aux[F[_], C <: Cap @uncheckedCaptures] = Monad[F] { type Ctx = C }

class Runner[F[_], C <: Cap @uncheckedCaptures](
    using val m: Monad.Aux[F, C] @uncheckedCaptures):
  transparent inline def run[T](inline t: T): F[T] =
    m.pure[T](t)

transparent inline def runner[F[_]](using m: Monad[F]) =
  new Runner(using m)
