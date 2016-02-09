// i1050 checks failing at refchecks
object Tiark1 {
    trait A { type L <: Nothing }
    trait B { type L >: Any}
    trait U {
      val p: B
      def brand(x: Any): p.L = x
    }
    trait V extends U {
      lazy val p: A & B = ??? // error: may not override
    }
    val v = new V {}
    v.brand("boom!")
}
object Tiark2 {
    trait A { type L <: Nothing }
    trait B { type L >: Any}
    trait U {
      type X <: B
      val p: X
      def brand(x: Any): p.L = x
    }
    trait V extends U {
      type X = B & A
      lazy val p: X = ???  // error: may not override
    }
    val v = new V {}
    v.brand("boom!"): Nothing
}

