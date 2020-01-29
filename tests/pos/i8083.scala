trait A {
  class B
}
abstract class Test2 { val a: A; val b: a.B }
object Test2 {
  def unapply(that: Test2): Option[(that.a.type, that.a.B)] = Some((that.a, that.b))
}
object anA extends A
object Test extends App {
  val t: Test2 { val a: anA.type; val b: anA.B } = new Test2 { val a: anA.type = anA; val b = new a.B }
  t match {
    case Test2(u, v) =>
      u: A
      u: t.a.type // error
      v: t.a.B // error
  }
}
object Test1 extends App {
  object t extends Test2 { val a = anA; val b = new a.B }
  t match {
    case Test2(u, v) =>
      u: A
      u: t.a.type // error
      v: t.a.B // error
  }
}