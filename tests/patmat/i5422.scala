import scala.language.higherKinds

object Test {

  trait X
  case object Y extends X

  sealed trait Foo[F[_], O] {
    def bar: Foo[F, O] = this match {
      case Qux(fa, f) => qux(fa) {
        case Left(Y) => ???
        case x => ???
      }
    }
  }

  case class Qux[F[_], A, O](fa: F[A], f: Either[X, A] => Int) extends Foo[F, O]

  def qux[F[_], A, O](fa: F[A])(f: Either[X, A] => Int): Foo[F, O] =
    Qux(fa, f)
}
