import scala.annotation.valhalla

@valhalla
abstract class AbstractVVC extends AnyVal {
  def addOne(x: Int): Int
}

@valhalla
class VVC extends AbstractVVC {
  def addOne(x: Int): Int = x + 1
}