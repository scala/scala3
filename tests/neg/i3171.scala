object Test {
  class C(x: Int = 1, y: Int) {
    def this(x: Int = 1)(y: String) = // error: two or more overloaded methods have default getters
      this(x, y.toInt)
  }

  def test: Unit = {
    new C()("1")
  }
}
