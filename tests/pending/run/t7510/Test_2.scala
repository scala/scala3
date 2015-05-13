import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends dotty.runtime.LegacyApp {
  val tb = cm.mkToolBox()
  tb.compile(tb.parse("@foo.Ann_1 class C"))
}

