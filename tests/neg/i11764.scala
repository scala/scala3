import deriving.Mirror

// a sealed trait with inaccessible children should
// still not be a generic sum.
sealed trait Foo
object Foo {
  def local =
    case class Local() extends Foo
}

// in `3.0-3.1`, sealed abstract case class was neither a
// generic sum type (because it can't have `case` children),
// or a generic product type (because we can't call the ctor),
// but now sum types can have no children,
// so we must restrict against case classes.
sealed abstract case class Bar()

def foo =

  val mFoo = summon[Mirror.SumOf[Foo]] // error

  val mBar = summon[Mirror.SumOf[Bar]] // error
  val mBaz = summon[Mirror.ProductOf[Bar]] // error
