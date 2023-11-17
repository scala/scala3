trait Arb[Fx[_]] {
  def pure[A](x: A): Fx[A]
}

class PfOps(private val self: Int) extends AnyVal {
  def pf[Fy[_]](m: Arb[Fy]): PartialFunction[Int, Fy[Int]] = {
    case x => m.pure(x)
  }
}
