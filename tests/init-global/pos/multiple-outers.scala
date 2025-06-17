class A(val x: Int) {
  class B {
    println("A.this = " + A.this.hashCode()) // `a`
    println("A.this.x = " + A.this.x) // B --> outer A (42 or 46)
    def fooz = x
    def fooz2 = x
    class D {
      println("B.this = " + B.this.hashCode()) // `c` in `foo`
      def bar = fooz // expands to B.this.fooz, calls fooz in class B
      def bar2 = fooz2 // expands to B.this.fooz, calls fooz2 in class C
    }
  }
}
class AA(y: Int) extends A(y+1) {
  class E {}
  def foo = {
    val a = if true then new A(42) else new AA(46)
    println("a = " + a.hashCode())
    class C /*outer: AA(44) (`Main.aa`)*/ extends a.B /*outer: A(42) or AA(46) (`a`)*/ {
      println("AA.this = " + AA.this.hashCode()) // Main.aa
      println("AA.this.x = " + x) // C --> outer AA --> parent A (44)
      override def fooz2 = x // 44
      val z = fooz // (A.this.x)
      println("z = " + z)

    }
    class B extends AA.this.E {}
    val c: C = new C
    println("c = " + c.hashCode())
    val d = new c.D // outer: C (`c`)
    println("d.bar = " + d.bar + ", d.bar2 = " + d.bar2)
    d.bar + d.bar2
  }
}

object O {
  val aa = new AA(44)
  val f = aa.foo
  println("aa = " + aa.hashCode())
  println("f = " + f)
}

object Main {
  def main(args: Array[String]) = {
    O
    ()
  }
}