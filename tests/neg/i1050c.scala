// i1050 checks failing at typer
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
  type Y >: Any <: Nothing  // error: only classes can have declared but undefined members
}
