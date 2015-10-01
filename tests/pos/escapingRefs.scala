class Outer {
  class Inner
}

object Test {
  def test = {
    val a: Outer#Inner = {
      val o = new Outer
      new o.Inner
    }
  }
}
