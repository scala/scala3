trait Foo[-L] {
  def test[LF, F[_ >: L] >: LF]: Unit
}