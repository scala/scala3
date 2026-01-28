import scala.annotation.valhalla

@valhalla
class ConcreteVVC(val a: Int, val b: Int) extends AnyVal {
  def addOne(x: Int): Int = x + 1
}