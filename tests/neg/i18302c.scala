import scala.reflect.Selectable.reflectiveSelectable

def test = polyFun.foo(1)
def polyFun: PolyFunction { def foo(x: Int): Int } = // error
  new PolyFunction { def foo(x: Int): Int = x + 1 } // error
