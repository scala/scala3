import scala.deriving.Mirror

package lib {
  enum Foo:
    case A

  case object Bar
}

package app {
  object Foo:
    val A: lib.Foo.A.type = lib.Foo.A
  val Bar: lib.Bar.type = lib.Bar
}

@main def Test =
  assert(summon[Mirror.Of[scala.Nil.type]].fromProduct(EmptyTuple) == Nil) // alias scala 2 defined
  assert(summon[Mirror.Of[lib.Foo.A.type]].fromProduct(EmptyTuple) == lib.Foo.A) // real mirror
  assert(summon[Mirror.Of[lib.Bar.type]].fromProduct(EmptyTuple) == lib.Bar) // real mirror
  assert(summon[Mirror.Of[app.Foo.A.type]].fromProduct(EmptyTuple) == lib.Foo.A) // alias mirror
  assert(summon[Mirror.Of[app.Bar.type]].fromProduct(EmptyTuple) == lib.Bar) // alias mirror
