object internal:
  trait Functor[F[_]] {
    extension [T](ft: F[T]) def map[T1](f: T => T1): F[T1]
  }

object cats:
  trait Functor[F[_]]
  object Functor:
    trait Ops[F[_], A]:
      def map[B](f: A => B): F[B] = ???
    def toAllFunctorOps[F[_], A](target: F[A])(using Functor[F]): Ops[F, A] = ???

given [F[_]](using cf: cats.Functor[F]): internal.Functor[F] with {
  extension [T](ft: F[T]) def map[T1](f: T => T1): F[T1] = ???
}

trait Ref[F[_], +T]
class MemoizingEvaluator[Input[_[_]], Output[_[_]], F[_]: cats.Functor] {
  type OptionRef[T] = Ref[F, Option[T]]

  def sequence[CaseClass[_[_]], G[_], H[_]](instance: CaseClass[[t] =>> G[H[t]]]): G[CaseClass[H]] = ???
  def collectValues(input: Input[F]): F[(Input[F], Input[OptionRef])] = {
    val refsF: Input[[t] =>> F[OptionRef[t]]] = ???
    for {
      refs <- cats.Functor.toAllFunctorOps(sequence[Input, F, OptionRef](refsF))
      updating = ???
    } yield (updating, refs)
  }
}
