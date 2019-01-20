class Test {
  def foo: String = ""
  def test(cond: Boolean): Int = {
    if (cond) foo
    1
  }

  def test2(cond: Boolean): Unit = {
    val x = if (cond) foo
  }

  def test3(cond: Boolean): Unit = {
    val x: Unit = if (cond) foo
  }
}
