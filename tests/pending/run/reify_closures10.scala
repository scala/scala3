import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends dotty.runtime.LegacyApp {
  val x = 2
  val y = 3
  val code = reify{println(x + y); x + y}

  val toolbox = cm.mkToolBox()
  println(toolbox.eval(code.tree))
}
