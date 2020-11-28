class C
given c as C with
  def foo = 1

given d as C = new C { def foo = 1 }

def test =
  c.foo  // OK
  d.foo  // error

