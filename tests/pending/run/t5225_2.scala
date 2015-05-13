import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  val tree = reify{def foo(@annotation.elidable(0) x: Int) = ""}.tree
  println(tree.toString)
}
