trait T {
  inline def foo(handler: Int): Unit =
    bar(handler)

  private inline def bar(handler: Int): Unit = ()
}

def test = new T {
  foo(42)
  this.foo(42)
  def test = this.foo(42)
}

def test2(t: T) =
  t.foo(42)
