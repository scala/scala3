
sealed abstract class LazyEither[A, B] {

  def fold[X](left: (=> A) => X, right: (=> B) => X): X =
    this match {
      case LazyLeft(a)  => left(a())
      case LazyRight(b) => right(b())
    }
}

object LazyEither {

  final case class LeftProjection[A, B](e: LazyEither[A, B]) extends AnyVal {

    def getOrElse[AA >: A](default: => AA): AA =
      e.fold(z => z, _ => default)
  }
}
private case class LazyLeft[A, B](a: () => A) extends LazyEither[A, B]
private case class LazyRight[A, B](b: () => B) extends LazyEither[A, B]
