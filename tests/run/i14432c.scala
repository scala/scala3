import deriving.Mirror

package example {
  case class Foo protected [example] (val i: Int)

  // case object companion here prevents Foo from caching
  // the mirror in its companion, so all potential mirrors for Foo will be anonymous.
  case object Foo

  class Bar extends Foo(23) {
    class Inner {
      // here, we can synthesize an anonymous mirror
      // because at this call site the constructor is accessible.
      val mFoo = summon[Mirror.Of[example.Foo]]
    }
    val inner = Inner()
  }

}

@main def Test: Unit =
  val bar = new example.Bar
  assert(bar.inner.mFoo.fromProduct(Tuple1(1)).i == 1)
