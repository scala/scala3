import scala.annotation.valhalla

@valhalla
abstract class AbstractVVC(val a: Int, val b: Int) extends AnyVal {
  def addOne(x: Int):Int = x
}