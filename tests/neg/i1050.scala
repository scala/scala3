trait A { type L <: Nothing }
trait B { type L >: Any}
object Test {
  lazy val x: A & B = ???
  val y: x.L = 1    // error: underlying conflicting bounds
  val z: String = y
}
object Test50 {
  trait A {
    type X = String
  }
  trait B {
    type X = Int
  }
  lazy val o: A & B = ???

  def xToString(x: o.X): String = x // error: underlying conflicting bounds

  def intToString(i: Int): String = xToString(i)

  def main(args: Array[String]) = {
    val s: String = intToString(1)
  }
}
object Test2 {

  trait C { type A }

  type T = C { type A = Any }
  type U = C { type A = Nothing }
  type X = T & U

  def main(args: Array[String]) = {
    val y: X#A = 1 // error: conflicting bounds
    val z: String = y
  }
}
object Tiark1 {
    trait A { type L <: Nothing }
    trait B { type L >: Any}
    trait U {
      lazy val p: B
      def brand(x: Any): p.L = x // error: nonfinal lazy
    }
    trait V extends U {
      lazy val p: A & B = ???
    }
    val v = new V {}
    v.brand("boom!")
}
object Tiark2 {
    trait A { type L <: Nothing }
    trait B { type L >: Any}
    trait U {
      type X <: B
      lazy val p: X
      def brand(x: Any): p.L = x // error: nonfinal lazy
    }
    trait V extends U {
      type X = B & A
      lazy val p: X = ???
    }
    val v = new V {}
    v.brand("boom!"): Nothing
}
object Tiark3 {
    trait A { type L <: Nothing }
    trait B { type L >: Any}
    trait U {
      type X <: B
      def p2: X
      final lazy val p: X = p2
      def brand(x: Any): p.L = x // error: underlying not concrete
    }
    trait V extends U {
      type X = B with A
      def p2: X = ???
    }
    val v = new V {}
    v.brand("boom!"): Nothing
}
object Tiark4 {
    trait U {
      type Y
      trait X { type L = Y }
      def compute: X
      final lazy val p: X = compute
      def brand(x: Y): p.L = x
    }
    trait V extends U {
      type Y >: Any <: Nothing
      def compute: X = ???
    }
    val v = new V {} // error: cannot be instantiated
    v.brand("boom!")
}
object Import {
    trait A { type L <: Nothing }
    trait B { type L >: Any}
    trait U {
      lazy val p: B
      locally { val x: p.L = ??? } // error: nonfinal lazy
      locally {
        import p._
        val x: L = ??? // error: nonfinal lazy
      }
    }
}
object V { // error: cannot be instantiated
  type Y >: Any <: Nothing  // error: only classes can have declared but undefined members
}
object Tiark5 {
    trait A { type L <: Nothing }
    trait B { type L >: Any }
    def f(x: => A & B)(y: Any):Nothing = (y:x.L) // error: underlying conflicting bounds
    f(???)("boom!")
}
object Tiark5Inherited {
    trait A { type L <: Nothing }
    trait B { type L >: Any }
    trait A2 extends A
    trait B2 extends B
    def f(x: => A2 & B2)(y: Any):Nothing = (y:x.L) // error: underlying conflicting bounds
    f(???)("boom!")
}
object Tiark6 {
    trait B { type L >: Any }
    trait A { type L <: Nothing }
    trait U {
      trait X {
        val q: A & B = ???
      }
      final lazy val p: X = ???
      def brand(x: Any): p.q.L = x // error: conflicting bounds
    }
    val v = new U {}
    v.brand("boom!"): Nothing
}
