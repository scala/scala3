object i11924:
  type A
  class B { def foo = 0 }
  trait Ev { type T >: A <: B }

  inline def test(ev: Ev)(x: ev.T): Int = x.foo

  def trial(ev: Ev, a: A) = {
    test(ev)(a)
  }
