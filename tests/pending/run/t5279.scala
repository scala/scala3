import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  reify {
    println(new Integer(10))
  }.eval
}
