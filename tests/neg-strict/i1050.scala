// i1050 checks failing at posttyper
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
      type X = B & A
      def p2: X = ???
    }
    val v = new V {}
    v.brand("boom!"): Nothing
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

object Indirect {
  trait B { type L >: Any }
  trait A { type L <: Nothing }
  trait U {
    trait X {
      val q: A & B = ???
      type M = q.L
    }
    final lazy val p: X = ???
    def brand(x: Any): p.M = x // error: conflicting bounds
  }
  def main(args: Array[String]): Unit = {
    val v = new U {}
    v.brand("boom!"): Nothing
  }
}
object Indirect2 {
  trait B { type L >: Any }
  trait A { type L <: Nothing }
  trait U {
    trait Y {
      val r: A & B = ???
    }
    trait X {
      val q: Y = ???
      type M = q.r.L
    }
    final lazy val p: X = ???
    def brand(x: Any): p.M = x // error: conflicting bounds
  }
  def main(args: Array[String]): Unit = {
    val v = new U {}
    v.brand("boom!"): Nothing
  }
}
object Rec1 {
    trait B { type L >: Any }
    trait A { type L <: Nothing }
    trait U {
      trait Y {
        type L = Int
        val r: Y
      }
      trait X {
        val q: Y = ???
        type M = q.r.L  // if we are not careful we get a stackoverflow here
      }
    }
}
object Rec2 {
  trait B { type L >: Any }
  trait A { type L <: Nothing }
  trait U {
    trait Y {
      val r: A & B & Y
    }
    trait X {
      val q: Y = ???
      type M = q.r.L
    }
    final lazy val p: X = ???
    def brand(x: Any): p.M = x // error: conflicting bounds
  }
  def main(args: Array[String]): Unit = {
    val v = new U {}
    v.brand("boom!"): Nothing
  }
}
object Indirect3 {
  trait B { type L >: Any }
  trait A { type L <: Nothing }
  trait U {
    trait Y {
      val r: Y & A & B = ???
    }
    trait X {
      val q: Y = ???
      type M = q.r.L
    }
    final lazy val p: X = ???
    def brand(x: Any): p.M = x // error: conflicting bounds
  }
  def main(args: Array[String]): Unit = {
    val v = new U {}
    v.brand("boom!"): Nothing
  }
}
