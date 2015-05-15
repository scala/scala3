import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends dotty.runtime.LegacyApp {
  val tb = cm.mkToolBox()
  val c = tb.parse("object C")
  println(showRaw(tb.typecheck(c), printKinds = true))
}
