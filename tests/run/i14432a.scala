import deriving.Mirror

package example {
  case class Foo private[example] (val i: Int)

  // case object companion here prevents Foo from caching
  // the mirror in its companion, so all potential mirrors for Foo will be anonymous.
  case object Foo

  // here, we can synthesize an anonymous mirror
  // because at this call site the constructor is accessible.
  val mFoo = summon[Mirror.Of[example.Foo]]
}

@main def Test: Unit =
  assert(example.mFoo.fromProduct(Tuple1(1)).i == 1)
