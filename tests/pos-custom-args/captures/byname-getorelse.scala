import language.experimental.captureChecking

class Unlifted[A, B](f: A => Option[B]) extends scala.runtime.AbstractPartialFunction[A, B]:
  def isDefinedAt(x: A): Boolean = f(x).isDefined

  override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
    f(x).getOrElse(default(x))
