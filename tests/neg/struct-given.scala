class C
given c: [T] => C:
  def foo = 1

given d[T]: C = new C { def foo = 1 }

def test =
  c.foo  // OK
  d.foo  // error

