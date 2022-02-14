import quoted.*
class Container1(arg: Int):
  object Internal1:
    def exec: Unit = println("this is Internal1")
transparent inline def myC: Any = ${ macroCrap }
def macroCrap(using Quotes): Expr[Any] =
  '{ Container1(1) }