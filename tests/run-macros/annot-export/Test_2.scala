//> using options -experimental

object Bar:
  @returnClassName
  def f(): String = ??? // def f(): String = "Bar"

object Baz:
  export Bar.f // def f(): String = Bar.f(n)

@main def Test =
  assert(Bar.f() == "Bar")
  assert(Baz.f() == "Bar")
