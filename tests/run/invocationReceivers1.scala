trait T { override def clone(): Object = "hi" }
trait U extends T
class C1 extends U with Cloneable {
  // C1 gets a forwarder for clone that invokes T.clone. this is needed because JVM method
  // resolution always prefers class members, so it would resolve to Object.clone, even if
  // C1 is a subtype of the interface T which has an overriding default method for clone.

  // invokeinterface T.clone
  def f1 = (this: T).clone()

  // cannot invokeinterface U.clone (NoSuchMethodError). Object.clone would work here, but
  // not in the example in C2 (illegal access to protected). T.clone works in all cases and
  // resolves correctly.
  def f2 = (this: U).clone()

  // invokevirtual C1.clone()
  def f3 = (this: C1).clone()
}

class C2 {
  def f1(t: T) = t.clone()  // invokeinterface T.clone
  def f2(t: U) = t.clone()  // invokeinterface T.clone -- Object.clone would be illegal (protected, explained in C1)
  def f3(t: C1) = t.clone() // invokevirtual C1.clone -- Object.clone would be illegal
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

