trait Monad[F[_]] extends Invariant[F]

trait Invariant[F[_]]
object Invariant:
  implicit def catsInstancesForList: Monad[List] = ???
  implicit def catsInstancesForVector: Monad[Vector] = ???

trait Shrink[T]
object Shrink extends ShrinkLowPriorityImplicits:
  trait Buildable[T,C]
  implicit def shrinkContainer[C[_],T](implicit v: C[T] => Traversable[T], s: Shrink[T], b: Buildable[T,C[T]]): Shrink[C[T]] = ???
trait ShrinkLowPriorityImplicits:
  implicit def shrinkAny[T]: Shrink[T] = ???

trait Distribution[F[_], -P, X] extends (P => F[X])
type GenBeta[A, B, X] = [F[_]] =>> Distribution[F, Beta.Params[A, B], X]
type Beta[R] = [F[_]] =>> GenBeta[R, R, R][F]

object Beta:
  trait Params[+A, +B]
trait BetaInstances:
  given schrodingerRandomBetaForDouble: [F[_]: Monad] => Beta[Double][F] = ???

object all extends BetaInstances

@main def Test =
  import all.given
  summon[Shrink[Beta.Params[Double, Double]]]