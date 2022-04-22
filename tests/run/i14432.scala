import deriving.Mirror

package example {
  // Foo caches the mirror in its companion,
  // which can still access the constructor.
  case class Foo private (val i: Int)
}

@main def Test: Unit =
  val mFoo = summon[Mirror.Of[example.Foo]]
  assert(mFoo.fromProduct(Tuple1(1)).i == 1)
