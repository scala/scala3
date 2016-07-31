class B { def m: Int = 0 }
class C extends B { override def m = 4 }
trait T1 extends B { override def m = 1 }
trait T2 extends T1 { override def m = 2 }
trait T3 extends T1 { override def m = 3 }

trait T4 extends T1
class D extends B {
  def f() = println(super[B].m)
}

object Test extends C with T2 with T3 with T4 {
  def main(args: Array[String]): Unit = {
    println(m)
    println(super[T2].m)
    println(super[T3].m)
    println(super[T4].m)
    new D().f()
  }
}
