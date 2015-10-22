object Test {
  def oneOrTwo(x: 1 | 2): 1 | 2 = x
  def test: Unit = {
    val foo: 3 | 4 = 1 // error
    oneOrTwo(foo) // error
  }
}
