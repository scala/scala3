object Test {

  abstract class A {
    def f(): String = "A.f"
  }

  object B extends A {
    override inline def f(): String = "B.f"
  }

  def main(args: Array[String]): Unit = {
    val a: A = B
    assert(a.f() == "B.f")

    val b: B.type = B
    assert(b.f() == "B.f")
  }

}
