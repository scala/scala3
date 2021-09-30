opaque type Finally[A] = A

trait SomeTrait[F[_]] {
  def foo: F[Unit]
  def withTV[A]: F[A]
  def withTV2[A, B]: F[(A, B)]
}

given none: SomeTrait[Finally] with {} // error
