trait Foo[X]:
  def foo: Int

def foo =
  first[String] // error // before line 10 to test alignment of the error message `|`

transparent inline def second[A]: Int =
  compiletime.summonInline[Foo[A]].foo

transparent inline def first[A]: Int =
  second[A] + 42 // after line 10 to test alignment of the error message `|`
