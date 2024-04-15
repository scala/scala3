object Foo:
  def xyz[A, CC[X] <: Iterable[X]](coll: CC[A]): Unit = ()

object Bar:
  export Foo.xyz
