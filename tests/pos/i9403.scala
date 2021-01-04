import scala.reflect.Selectable.reflectiveSelectable

object Test {
  def main(args: Array[String]): Unit = {
    def fCompareToBoolean(x: { def compareTo(y: java.lang.Boolean): Int }, y: Boolean): Int =
      x.compareTo(y)
    assert(fCompareToBoolean(false, true) < 0)
  }
}