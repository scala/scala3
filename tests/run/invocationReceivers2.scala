trait T { override def clone(): String = "hi" }
trait U extends T
class C1 extends U with Cloneable {
  def f1 = (this: T).clone()

  def f2 = (this: U).clone()

  def f3 = (this: C1).clone()
}

class C2 {
  def f1(t: T) = t.clone()
  def f2(t: U) = t.clone()
  def f3(t: C1) = t.clone()
}

object Test {
  def main(arg: Array[String]): Unit = {
    val r = new StringBuffer()
    val c1 = new C1
    r.append(c1.f1)
    r.append(c1.f2)
    r.append(c1.f3)
    val t = new T { }
    val u = new U { }
    val c2 = new C2
    r.append(c2.f1(t))
    r.append(c2.f1(u))
    r.append(c2.f1(c1))
    r.append(c2.f2(u))
    r.append(c2.f2(c1))
    r.append(c2.f3(c1))
    r.toString
  }
}

