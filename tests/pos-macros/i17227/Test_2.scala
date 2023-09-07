def g(i: Int): Int = i

def test =
  foo(f = g)
  bar(f = g)
  baz(f = g)
