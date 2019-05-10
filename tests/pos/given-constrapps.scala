class TC
val tc = TC()
class C given (x: TC) {
  assert(x eq tc)
}
class C2(n: Int) given (x: TC) given List[TC] {
  assert(x eq tc)
  the[List[TC]].foreach(t => assert(t eq tc))

  def this() given TC given List[TC] = this(1)
}

class D extends (C given tc)
class D2 extends (C2(1) given tc given Nil)

class Foo given TC {
  assert(the[TC] != null)
}

object Test extends App {
  new (C given tc)
  new (C() given tc)
  new (C given tc) {}
  new (C2(1) given tc given List(tc))
  new (C2(1) given tc given List(tc)) {}
  new (C2() given tc given List(tc))
  def foo given TC = ()
  foo given tc
}