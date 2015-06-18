import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  reify {
    println("%s %s %s".format(List("a", "b", "c"): _*))
  }.eval
}
