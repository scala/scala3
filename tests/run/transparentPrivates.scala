object Test {

  class C[T](private val x: T) {

    private def foo[Z](z: Z): T = x

    private var y: T = _

    transparent def get1 = x
    transparent def get2[U](c: C[U]) = c.x

    transparent def foo1(x: Int) = foo(x)
    transparent def foo2[U](c: C[U]) = c.foo(x)

    transparent def set1(z: T) = { y = z; y }
    transparent def set2[U](c: C[U]) = { c.y = c.x; c.y }
  }

  object CC {
    private val x = 3
    transparent def get1 = x
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
