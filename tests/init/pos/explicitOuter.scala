class Outer(elem: Int, val next: Outer) {
  def inner2 = {
    class C {
      val x = elem
    }
    class D {
      new C
    }
  }
}
