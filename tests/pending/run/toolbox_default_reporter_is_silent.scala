import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends dotty.runtime.LegacyApp {
  val toolbox = cm.mkToolBox()
  toolbox.eval(reify{
    object Utils {
      @deprecated("test", "2.10.0")
      def foo: Unit = { println("hello") }
    }

    Utils.foo
  }.tree)
}
