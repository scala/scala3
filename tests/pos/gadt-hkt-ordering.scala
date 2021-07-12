object test {
  final class HKTVar[+T[_]]

  def foo[F[_], G[_], X](m : HKTVar[G]) = m match {
    case _ : HKTVar[F] =>
      val fx : F[X] = ???
      val gx : G[X] = fx
  }
}
