package example

import com.javacp

class MetacJava {
  javacp.MetacJava.StaticInner.isStatic()
  new javacp.MetacJava.StaticInner().isNotStatic()
  val inner = new javacp.MetacJava()
  val overload1 = new inner.Overload1()
  val overload2 = new inner.Overload2()
  inner.overload(new overload1.A())
  inner.overload(new overload2.A())
  val staticInner = new javacp.MetacJava.StaticInner()
  val nonStatic = new staticInner.NonStatic()
  nonStatic.method(nonStatic)
  javacp.MetacJava.overload(new javacp.MetacJava.Overload3.A())
  val interface: javacp.Interface = null
  val coin: javacp.Coin = javacp.Coin.PENNY
  val entry: java.util.Map.Entry[Int, Int] = null
}
