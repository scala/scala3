object Tiark1 {
    trait A { type L <: Nothing }
    trait B { type L >: Any}
    trait U {
      val p: B
      def brand(x: Any): p.L = x // error: not final
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
      val p: X
      def brand(x: Any): p.L = x // error: not final
    }
    trait V extends U {
      type X = B & A
      lazy val p: X = ???
    }
    val v = new V {}
    v.brand("boom!"): Nothing
}

