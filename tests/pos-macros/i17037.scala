import scala.quoted.*

class Foo:
  type Bar = Int

def macroImpl(using Quotes) =
  val foo = new Foo
  Type.of[foo.Bar]
