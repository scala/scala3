class Foo:
  def foo = 23

object Unrelated:
  extension (f: Foo)
    def g = f.foo // OK

  extension (f: Foo)
    def h1: Int = 0
    def h2: Int = h1 + 1  // OK
    def ++: (x: Int): Int = h2 + x  // OK