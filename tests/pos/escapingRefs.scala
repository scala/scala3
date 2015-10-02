class Outer {
  class Inner {
    class Inner2
  }
}

object Test {
  def test = {
    val a: Outer#Inner = {
      val o = new Outer
      new o.Inner
    }

    val b: Outer#Inner#Inner2 = {
      val o = new Outer
      val i = new o.Inner
      new i.Inner2
    }
  }
}
