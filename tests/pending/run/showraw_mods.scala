import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  val tree = reify{trait C { private[this] val x = 2; var y = x; lazy val z = y }}
  println(showRaw(tree.tree))
}
