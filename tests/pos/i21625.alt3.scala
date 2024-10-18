trait Foo[-L] {
  def test[F[_ >: L] >: Nothing]: Unit
}
