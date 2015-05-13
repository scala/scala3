import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  val x = 2
  val code = reify{
    {
      val inner = reify{reify{x}}
// was:      inner.splice
      inner.eval
// was:    }.splice
    }.eval
  }

  val toolbox = cm.mkToolBox()
  val evaluated = toolbox.eval(code.tree)
  println("evaluated = " + evaluated)
}
