trait Foo[A]:
  inline def foo(): Unit = bar[this.type](this)
  inline def bar[E](ref: Foo[A]): Unit = ${ impl[E]('ref) }
def test(p: Foo[Int]) = p.foo()
