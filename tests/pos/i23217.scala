trait HasA[T]:
  type A = T

object Test1:
  def foo1(h: HasA[?])(a: h.A): Unit = {}

  def foo2(h1: HasA[?])(a1: h1.A): Unit =
    foo1(h1)(a1)

  def foo3(h1: HasA[?], a1: h1.A): Unit =
    foo1(h1)(a1)

object Test2:
  def bar1(h: HasA[?], a: h.A): Unit = {}

  def bar2(h1: HasA[?], a1: h1.A): Unit =
    bar1(h1, a1)

  def foo3(h1: HasA[?])(a1: h1.A): Unit =
    bar2(h1, a1)
