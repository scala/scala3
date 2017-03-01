// i1050 checks failing at typer
object Import {
    trait A { type L <: Nothing }
    trait B { type L >: Any}
    trait U {
      lazy val p: B
      locally { val x: p.L = ??? } // old-error: nonfinal lazy
      locally {
        import p._ // error: Import.B(U.this.p) is not a legal path
        val x: L = ??? // old-error: nonfinal lazy
      }
    }
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
object V { // error: cannot be instantiated
  type Y >: Any <: Nothing  // old-error: only classes can have declared but undefined members
  type Z
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
object Tiark7 {
    trait A { type L <: Nothing }
    trait B { type L >: Any }
    def f(x: => B)(y: Any):x.L = y // error: x is not stable
    def f1(x: => A & B)(y: Any):Nothing = f(x)(y) // error: type mismatch
    f1(???)("boom!"): Nothing
}
