import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

class Expression {
  override def toString = "Expression"
}

object Test extends dotty.runtime.LegacyApp {
  val code = reify {
    List(new Expression, new Expression)
  };

  val toolbox = cm.mkToolBox()
  val evaluated = toolbox.eval(code.tree)
  println("evaluated = " + evaluated)
}
