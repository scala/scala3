import deriving.Mirror

sealed trait Foo
object Foo

sealed trait Bar


@main def Test =
  val mFoo = summon[Mirror.SumOf[Foo]]
  assert(mFoo eq Foo)
  summon[mFoo.MirroredElemTypes =:= EmptyTuple]

  val mBar = summon[Mirror.SumOf[Bar]] // also check anonymous case
  summon[mBar.MirroredElemTypes =:= EmptyTuple]
