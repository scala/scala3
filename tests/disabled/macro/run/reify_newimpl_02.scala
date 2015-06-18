import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  {
    var x = 2
    val code = reify {
      x
    }
    println(code.eval)
  }
}
