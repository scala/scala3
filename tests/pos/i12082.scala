trait ReproTrait[F[_], E] {
  def reproFunction[A](pf: PartialFunction[E, F[Unit]]): Unit = ???
}

class ReproExtensionClass[F[_], A](val dummy: Boolean) extends AnyVal {
  def reproExtensionFunction(me: ReproTrait[F, Throwable]): Unit =
    me.reproFunction { case _ =>
      ???
    }
}
