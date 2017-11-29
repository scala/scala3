class Outer {
  class Inner {
    class Inner2
  }
}

class HasA { type A }

class Foo[A]

object Test {
  def test = {
    val a: Outer#Inner = {
      val o = new Outer
      new o.Inner
    }

    val b: Outer#Inner#Inner2 = {
      val o = new Outer
      val i = new o.Inner
      new i.Inner2
    }

    val c: HasA { type A = Int } = {
      val h = new HasA {
        type A = Int
      }
      val x: HasA { type A = h.A } = h
      x
    }

/*    val d: Foo[Int] = {
      class Bar[B] extends Foo[B]
      new Bar[Int]
    }
*/
    val e: Foo[_] = {
      class Bar[B11] extends Foo[B11]
      new Bar[Int]: Bar[_ <: Int]
    }
  }
}
