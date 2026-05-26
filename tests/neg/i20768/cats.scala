package cats

trait Monad[F[_]]

object Monad:
  implicit def catsInstancesForList: Monad[List] = ???
  implicit def catsBimonadForFunction0: Bimonad[Function0] = ???

trait Bimonad[F[_]] extends Monad[F]

class Eval[+A]
object Eval:
  implicit val catsMonadForEval: Monad[Eval] = new Monad[Eval] {}

package data:
  class IndexedStateT[F[_], SA, SB, A]:
    def flatMap[B, SC](fas: A => IndexedStateT[F, SB, SC, B])(implicit F: Monad[F]): IndexedStateT[F, SA, SC, B] = ???

  object IndexedStateT:
    implicit def catsDataMonadForIndexedStateT[F[_], S](implicit F0: Monad[F]): Monad[[A] =>> IndexedStateT[F, S, S, A]] =
      new Monad[[A] =>> IndexedStateT[F, S, S, A]] {
        def pure[A](a: A): IndexedStateT[F, S, S, A] = ???
        def flatMap[A, B](fa: IndexedStateT[F, S, S, A])(f: A => IndexedStateT[F, S, S, B]): IndexedStateT[F, S, S, B] = ???
      }

  type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
  object StateT:
    def set[F[_], S](s: S)(implicit F: Monad[F]): StateT[F, S, Unit] = ???
    def pure[F[_], S, A](a: A)(implicit F: Monad[F]): StateT[F, S, A] = ???
    def get[F[_], S](implicit F: Monad[F]): StateT[F, S, S] = ???
