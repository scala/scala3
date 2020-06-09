sealed trait Test[+F[_], +A] extends Product with Serializable

object Test {

  implicit class Syntax[F[_], A](val self: Test[F, A]) extends AnyVal {

    def fold[B](completed: F[A] => B): B = self match {
      case Completed(fa) => completed(fa) // error
    }
  }


  final case class Completed[F[_], A](fa: F[A]) extends Test[F, A]
}
