object Test {
  def test: Unit = {
    val x = iDontExist(32) // error: not found: iDontExist
    val y = x
    x + x
    println(x)
  }
}
