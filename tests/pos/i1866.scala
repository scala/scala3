import scala.reflect.Selectable.reflectiveSelectable
object Test {
  def f(g: { val update: Unit }) = g.update
  def main(update: Array[String]) = {}
}
