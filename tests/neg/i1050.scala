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
      lazy val p: X
      def brand(x: Any): p.L = x // error: not final
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
      def brand(x: Any): p.L = x
    }
    trait V extends U {
      type X = B with A
      def p2: X = ???
    }
    val v = new V {}
    v.brand("boom!"): Nothing
}
/*
object Import {
    trait A { type L <: Nothing }
    trait B { type L >: Any}
    trait U {
      val p: B
      def brand(x: Any): p.L = x // error: not final
      locally { import p._
      }
    }
    trait V extends U {
      lazy val p: A & B = ???
    }
    val v = new V {}
    v.brand("boom!")
}
*/
