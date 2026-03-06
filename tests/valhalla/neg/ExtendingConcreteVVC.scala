import scala.annotation.valhalla

@valhalla
class ConcreteVVC extends AnyVal {
  def addOne(x: Int): Int = x + 1
}

@valhalla
class VVC extends ConcreteVVC // error

class Ident extends ConcreteVVC // error