object Test {

  class C[T](private val x: T) {

    private def foo[Z](z: Z): T = x

    private var y: T = _

    rewrite def get1 = x
    rewrite def get2[U](c: C[U]) = c.x

    rewrite def foo1(x: Int) = foo(x)
    rewrite def foo2[U](c: C[U]) = c.foo(x)

    rewrite def set1(z: T) = { y = z; y }
    rewrite def set2[U](c: C[U]) = { c.y = c.x; c.y }
  }

  object CC {
    private val x = 3
    rewrite def get1 = x
  }

  def main(args: Array[String]) = {
    val cc = new C(2)
    assert(cc.get1 == 2)
    assert(cc.get2(cc) == 2)
    assert(cc.foo1(1) == 2)
    assert(cc.foo2(cc) == 2)
    assert(cc.set1(3) == 3)
    assert(cc.set2(cc) == 2)

    assert(CC.get1 == 3)
  }

}
