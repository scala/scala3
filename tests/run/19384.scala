object Test {

  def main(args: Array[String]): Unit = {
    Left(()).recoverWith { _ =>
      List.empty[Int].find(_ == 1) match {
        case None    => Left(())
        case Some(_) => Right(())
      }
    }
  }
}

implicit final def catsSyntaxEither[A, B](eab: Either[A, B]): EitherOps[A, B] = new EitherOps(eab)
final class EitherOps[A, B](private val eab: Either[A, B]) extends AnyVal {
  def recoverWith[AA >: A, BB >: B](pf: PartialFunction[A, Either[AA, BB]]): Either[AA, BB] =
    eab match {
      case Left(a) if pf.isDefinedAt(a) => pf(a)
      case _                            => eab
    }
}
