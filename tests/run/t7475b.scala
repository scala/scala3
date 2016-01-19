trait A { private val x = 1 }
trait B { val x = 2 }
trait C { val x: Int }
trait C1 extends B with A { println(x) }
trait C2 extends A with B { println(x) }
trait C3 extends C with B { println(x) }
trait C4 extends B with C { println(x) }

object Test {
  def main(args: Array[String]): Unit = {
    new C1 { }
    new C2 { }
    new C3 { }
    new C4 { }
  }
}
