package cek

trait Async[F[_]]

object Async {
  trait WriterTAsync[F[_], L1]
      extends Async[({ type LL[A] = WriterT[F, L1, A] })#LL]
      with MonadCancel.WriterTMonadCancel[F, L1] {

    override def delegate = super.delegate
  }

}

case class WriterT[F[_], L0, V]()

trait MonadError[F[_]]
trait MonadCancel[F[_]]

object MonadCancel {

  trait WriterTMonadCancel[F[_], L2]
      extends MonadCancel[({ type LL[A] = WriterT[F, L2, A] })#LL] {

    def delegate: MonadError[({ type LL[A] = WriterT[F, L2, A] })#LL] =
      ???

  }
}
