import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends dotty.runtime.LegacyApp {
  val tb = cm.mkToolBox()
  val expr = tb.parse("1 to 3 map (_+1)")
  println(tb.typecheck(expr))
}
