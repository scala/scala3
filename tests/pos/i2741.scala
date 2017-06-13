object Result {
  type ResultF[F] = F | Errors
  type ResultF2[F] = F & Errors

  case class Errors(msgs: Seq[String])

  implicit class ResultFunctions[A](val res: ResultF[A]) extends AnyVal {
    def map[B](f: A => B): ResultF[B] = res match {
      case errs: Errors => errs
      case x: A => f(x)
    }
  }
  implicit class ResultFunctions2[A](val res: ResultF2[A]) extends AnyVal {
    def map[B](f: A => B): ResultF[B] = ???
  }
}
