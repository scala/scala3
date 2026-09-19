trait T { override def clone(): Object = "hi" }
trait U extends T
class C1 extends U with Cloneable {
  def f2 = (this: U).clone()
}
class C2 {
  def f2(t: U) = t.clone()
}

object Test:
  def main(args: Array[String]): Unit =
    println(new C1().clone())
    println(new C2().f2(new C1()))
