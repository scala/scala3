import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  class C {
    type T = Int
    val code = reify {
      List[C#T](2)
    }
    println(code.eval)
  }

  new C
}
