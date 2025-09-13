package intersectionTypes

object t1 {
  trait Resetable {
    def reset(): this.type
  }

  trait Growable[T] {
    def add(x: T): this.type
  }

  def f(x: Resetable & Growable[String]) = {
    x.reset()
    x.add("first")
  }
}

object t2 {

  trait A {
    def children: List[A]
  }

  trait B {
    def children: List[B]
  }

  val x: A & B = new C
  val ys: List[A] & List[B] = x.children

  class C extends A with B {
    def children: List[A & B] = ???
  }

  val z: B & A = x // commutative
}
