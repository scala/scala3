
class A:
  val m: A^ = ???
  val self: this.type = this

case class C(ca: A^)

def test1(a: A^, b: A^) =
  val c1: A^{a} = a.m
  val c2: A^{a.m} = a.m
  val c3: A^{b} = a.m // error

  val d1: A^{a} = a.self
  val d2: A^{a.self} = a.self
  val d3: A^{a.self} = a

  val e1: A^{a.m} = a.self.m
  val e2: A^{a.self.m} = a.self.m
  val e3: A^{a.self.m} = a.m

def test2(a: A^) =
  val b: a.type = a
  val c1: C^{a} = new C(a)
  val c2: C^{a} = new C(a.m)
  val c3: C^{a.m} = new C(a.m)
  val c4: C^{b} = new C(a)
  val c5: C^{a} = new C(b)