type Callback = CallbackTo[Unit]

final class CallbackTo[+A] { self =>

  def >>[B](y: CallbackTo[B]): CallbackTo[B] =
    ???

  inline def *>[B](z: CallbackTo[B]): CallbackTo[B] =
    >>(z)

  def qwe: CallbackTo[A] = {
    def x: Callback = ???
    val hmmm = this

    x *> this // was error
    x *> self // was error
    x *> hmmm // ok

    ???
  }
}
