
class A(b: B) {
  def this(b: B, m: Int) = {
    this(b)
    def foo = m // resolved to parameter `m`
    class C { foo } // resolved to parameter `m`, as hidden field of `A`
    new C
  }
}

class D(b: B) extends A(b, 10) {
  val n = 10
}

class B {
  val a = new D(this)
}
