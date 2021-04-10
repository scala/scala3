
class A {
  def get(): Int = 0
}

class B extends A {}

class C extends A {}

def test1 = {
  val s: String | Null = ???
  val l = s.length

  val a: A | Null = new A
  a.get()

  val bc: B | C = new B
  bc.get()

  val bcn: B | (C | Null) = new C
  bcn.get()

  val bnc: (B | Null) | C = null
  bnc.get()

  val abcn: A | B | C | Null = new A
  abcn.get()
}

def test2 = {
  val s: String | Nothing = ???
  val l = s.length

  val a: A | Nothing = new A
  a.get()

  val bc: B | C = new B
  bc.get()

  val bcn: B | (C | Nothing) = new C
  bcn.get()

  val bnc: (B | Nothing) | C = new B
  bnc.get()

  val abcn: A | B | C | Nothing = new A
  abcn.get()
}