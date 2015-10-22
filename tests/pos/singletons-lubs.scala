object Test {
  def oneOrTwo(x: 1 | 2): 1 | 2 = x
  def test: Unit = {
    val foo: 1 | 2 = 1
    oneOrTwo(oneOrTwo(foo))
    1 match {
      case x: (1 | 2) => oneOrTwo(x)
      //case x @ (1 | 2) => oneOrTwo(x) // disallowed to avoid deep subtyping checks
    }
  }
}
