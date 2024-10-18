trait Foo[-L] {
  def test[F[_ >: L] >: Unit]: Unit
}
