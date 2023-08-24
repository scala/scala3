class C
type Cap = C^

class Foo(x: Cap):
  this: Foo^{x} =>

def test(c: Cap) =
  val x = Foo(c)
  ()
