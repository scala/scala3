import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

trait O { trait I }

object A extends O {
  val impl = new I {}
}

object Test extends dotty.runtime.LegacyApp {
  val code = reify {
    val v: A.I = A.impl
    v
  }
  println(showRaw(code.tree))

  val v: A.I = code.eval
}
