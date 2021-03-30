class CallbackTo[A](val x: A):
  inline def runNow(): A = x
  inline def toScalaFn: () => A = () => runNow()
  def toOption(): Option[A] =
    val y: CallbackTo[Option[A]] = ???
    val f: () => Option[A] = y.toScalaFn // error
    f()