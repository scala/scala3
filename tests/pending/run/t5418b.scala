import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends dotty.runtime.LegacyApp {
  val tb = cm.mkToolBox()
  val untyped = reify(new Object().getClass).tree
  val typed = tb.typecheck(untyped)
  println(typed)
  println(showRaw(typed.tpe))
}
