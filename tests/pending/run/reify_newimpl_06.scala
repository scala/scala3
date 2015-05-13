import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  class C(val y: Int) {
    val code = reify {
      reify{y}.eval
    }
  }

  println(new C(2).code.eval)
}
