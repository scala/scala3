inline def foo(f: Int => Int): Int => Int = f
inline def bar(inline f: Int => Int): Int => Int = f
inline def baz(f: (Int => Int)*): Int => Int = f.head

def g(i: Int): Int = i

def test =
  foo(f = g)
  bar(f = g)
  baz(f = g)
