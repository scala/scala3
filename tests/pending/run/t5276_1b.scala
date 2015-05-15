import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  reify {
    implicit lazy val x = 2
    println(implicitly[Int])
  }.eval
}
