object Test extends App {

  class C(val s: Array[Int]) extends AnyVal {
    override def equals(that: Any) = that match {
      case that: C => s.deep == that.s.deep
      case _ => false
    }
  }

  val c = new C(Array(1, 2,3))

  assert(c `equals` new C(Array(1, 2, 3)))
  assert(c == (new C(Array(1, 2, 3)): Any))
  assert(c == new C(Array(1, 2, 3)))

}
