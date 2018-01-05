import scala.reflect.Selectable.reflectiveSelectable

object Test {
  def f(g: { val update: String }) = g update
  def main(args: Array[String]) = {}
}
