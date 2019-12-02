object Test extends App {

  class C(val s: Array[Int]) extends AnyVal {
    override def equals(that: Any) = that match {
      case that: C => s.toList == that.s.toList
      case _ => false
    }
  }

  def test1() = {
    val c = new C(Array(1, 2,3))

    assert(c `equals` new C(Array(1, 2, 3)))
    assert(c == (new C(Array(1, 2, 3)): Any))
    assert(c == new C(Array(1, 2, 3)))

    assert(new C(Array(1, 2, 3)) == c)
    assert((new C(Array(1, 2, 3)): Any) == c)
    assert(new C(Array(1, 2, 3)) == c)
  }

  trait Eql extends Any {
     def deep: Any
     override def equals(that: Any) = that match {
      case that: D => deep == that.s.toList
      case _ => false
    }
  }

  class D(val s: Array[Int]) extends AnyVal with Eql {
    def deep = s.toList
  }

  def test2() = {
    val c = new D(Array(1, 2,3))

    assert(c `equals` new D(Array(1, 2, 3)))
    assert(c == (new D(Array(1, 2, 3)): Any))
    assert(c == new D(Array(1, 2, 3)))

    assert(new D(Array(1, 2, 3)) == c)
    assert((new D(Array(1, 2, 3)): Any) == c)
    assert(new D(Array(1, 2, 3)) == c)
  }

  test1()
  test2()

}
