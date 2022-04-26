class TC
val tc = TC()
class C(using x: TC) {
  assert(x eq tc)
}
class C2(n: Int)(using x: TC)(using List[TC]) {
  assert(x eq tc)
  summon[List[TC]].foreach(t => assert(t eq tc))

  def this()(using TC)(using List[TC]) = this(1)
}

class D extends C(using tc)
class D2 extends C2(1)(using tc)(using Nil)

class Foo(using TC) {
  assert(summon[TC] != null)
}

object Test extends App {
  new C(using tc)
  new C(using tc) {}
  new C2(1)(using tc)(using List(tc))
  new C2(1)(using tc)(using List(tc)) {}
  new C2()(using tc)(using List(tc))
  def foo(using TC) = ()
  foo(using tc)
}