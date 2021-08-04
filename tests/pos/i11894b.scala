object CallbackTo {
  extension [A](self: CallbackTo[Option[A]]) {
    inline def asOption: Option[A] =
      self.toScalaFn()
  }
}

final class CallbackTo[A] (val x: List[A]) {

  inline def runNow(): A =
    x.head

  inline def toScalaFn: () => A =
    () => runNow()

  def map[B](f: A => B): CallbackTo[B] =
    ???

  def toOption: Option[A] = {
    val x = map[Option[A]](Some(_))
    val y = x: CallbackTo[Option[A]] // ok: type is what we expect
    y.asOption // error
  }
}