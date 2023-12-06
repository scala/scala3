class Foo:
  def foo = 23

object Unrelated:
  extension (f: Foo)
    def g = f.foo // OK

  extension (f: Foo)
    def h1: Int = foo     // error
    def h2: Int = h1 + 1  // OK
    def h3: Int = g       // error
    def ++: (x: Int): Int = h1 + x  // OK