trait K[A] {
  sealed trait T
  case class  C(x: Int) extends T
  case object O         extends T
}

object Hello {
  def f[A](k: K[A])(t: k.T) = {
    t match {
      case k.C(x) => ???
      case k.O    => ???
    }
  }
}
