import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  reify {
    println(new {def x = 2; def y = x * x}.y)
  }.eval
}
