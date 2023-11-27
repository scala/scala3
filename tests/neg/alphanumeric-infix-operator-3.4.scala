//> using options -Werror

import language.`3.4`

class Foo:
  def x(i: Int) = i
  infix def y(i: Int) = i

def test(foo: Foo): Unit =
  foo x 1 // error (because it was compiled with 3.4+)
  foo y 2 // ok: is marked as infix
