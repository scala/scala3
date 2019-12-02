object Test {
  sealed trait Cause[+E]

  object Cause {
    final case class Fail[E](value: E) extends Cause[E]
  }

  def fn(cause: Cause[Any]): String =
    cause match {
      case Cause.Fail(t: Throwable) => t.toString
      case Cause.Fail(any)          => any.toString
    }
}
