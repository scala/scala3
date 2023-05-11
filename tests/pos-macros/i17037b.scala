import scala.quoted.*

class Foo:
  type Bar = Int

def macroImpl(using Quotes) =
  val foo = Foo()
  Type.of[foo.Bar] match
    case '[foo.Bar] => '{true}
    case _ => '{false}
