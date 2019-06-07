object Test {

  abstract class A {
    inline def f1(): String = "A.f1"
    inline def f2(): String = "A.f2"
    def f3(): String = "A.f3"
  }

  object B extends A {
    override def f1(): String = "B.f1"
    override inline def f2(): String = "B.f2"
    override inline def f3(): String = "B.f3"
  }

  def main(args: Array[String]): Unit = {
    val a: A = B
    assert(a.f1() == "A.f1")
    assert(a.f2() == "A.f2")
    assert(a.f3() == "A.f3")

    val b: B.type = B
    assert(b.f1() == "B.f1")
    assert(b.f2() == "B.f2")
    assert(b.f3() == "B.f3")
  }

}
