object Foo:
  def bar(x : Bar.YOf[Any]): Unit = ???

trait K:
  type CType <: Bar.YOf[Any]
  def foo : K =
    val x : CType = ???
    x // was: error: Found: CType, Expected: K

object Bar:
  type YOf[T] = K { type M }
