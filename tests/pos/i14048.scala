class T:
  inline def foo(): Unit = bar()
  private inline def bar(): Unit = ()

def test(t: T) = t.foo()
