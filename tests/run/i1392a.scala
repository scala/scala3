trait A { def m = 1 }
class B extends A { override def m = 2 }
trait T extends A
class C extends B with T {
  def t1 = super[B].m
  def t2 = super.m
  def t3 = super[T].m
}
object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    assert(c.t1 == 2)
    assert(c.t2 == 2)
    assert(c.t3 == 1)
  }
}
