import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  reify {
    def p(implicit i: Int) = print(i)
    implicit val v = 2

    println(p)
    println(p(1))
  }.eval
}
