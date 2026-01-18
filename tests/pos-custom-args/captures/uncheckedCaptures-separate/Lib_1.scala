package ccbug

import language.experimental.captureChecking
import caps.Control
import scala.annotation.unchecked.uncheckedCaptures

trait CpsCapability extends Control
trait CpsMonadContext[F[_]] extends CpsCapability

trait CpsMonad[F[_]]:
  type Context <: CpsMonadContext[F] @uncheckedCaptures
  def apply[T](op: Context ?=> T): F[T]

object CpsMonad:
  type Aux[F[_], C <: CpsMonadContext[F] @uncheckedCaptures] =
    CpsMonad[F] { type Context = C }

class AsyncApply[F[_], C <: CpsMonadContext[F] @uncheckedCaptures](
    using val am: CpsMonad.Aux[F, C] @uncheckedCaptures):
  transparent inline def apply[T](inline body: C ?=> T): F[T] =
    am.apply[T](body)

transparent inline def async[F[_]](using am: CpsMonad[F]) =
  new AsyncApply(using am)
