object Obj {
  def apply[L]: Unit = ???

  extension (make: Unit) def apply(value: Int): String = ???

  def test: String = Obj[Int](1)
}
