class TC
val tc = TC()
class C with (x: TC) {
  assert(x eq tc)
}
class C2(n: Int) with (x: TC) with List[TC] {
  assert(x eq tc)
  summon[List[TC]].foreach(t => assert(t eq tc))

  def this() with TC with List[TC] = this(1)
}

class D extends C.with(tc)
class D2 extends C2(1).with(tc).with(Nil)

class Foo with TC {
  assert(summon[TC] != null)
}

object Test extends App {
  new C.with(tc)
  new C().with(tc)
  new C.with(tc) {}
  new C2(1).with(tc).with(List(tc))
  new C2(1).with(tc).with(List(tc)) {}
  new C2().with(tc).with(List(tc))
  def foo with TC = ()
  foo.with(tc)
}