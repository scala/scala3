import deriving.Mirror

package example {
  val mFoo = summon[Mirror.Of[Foo]] // ok, we can access Foo's ctor from here.
}

@main def Test: Unit =
  assert(example.mFoo.fromProduct(Some(23)) == example.Foo(23))
