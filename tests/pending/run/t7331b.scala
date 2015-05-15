import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}

object Test extends dotty.runtime.LegacyApp {
  val tb = cm.mkToolBox()
  try tb.parse("f(x")
  catch {
    case ToolBoxError(msg, _) => println(msg)
  }
}
