object Test {
  class C(x: Int, y: Int) {
    def this(x: Int = 1)(y: String) =
      this(x, y.toInt)
  }

  def test: Unit = {
    new C()("1")
  }
}
