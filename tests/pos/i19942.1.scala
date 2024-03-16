trait Alternative[F[_]]

opaque type Derived[A] = A
object Derived:
  extension [A](derived: Derived[A]) def instance: A = derived
  infix type <<<[F[_], G[_]] = [x] =>> F[G[x]]

import Derived.*
import scala.compiletime.summonInline

type DerivedAlternative[F[_]] = Derived[Alternative[F]]
object DerivedAlternative:
  inline def apply[F[_]]: Alternative[F] =
    import DerivedAlternative.given
    summonInline[DerivedAlternative[F]].instance
  given nested[F[_], G[_]]: DerivedAlternative[F <<< G] = ???

object auto:
  object alternative:
    transparent inline given [F[_]]: Alternative[F] = DerivedAlternative[F]

trait Test:
  import Test.*
  import auto.alternative.given
  val fails = summon[Alternative[OptList]]

// Fails if companion object defined AFTER trait
object Test:
  type OptList[A] = Option[List[A]]
