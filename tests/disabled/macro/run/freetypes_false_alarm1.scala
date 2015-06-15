import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  reify {
    val ru = scala.reflect.runtime.universe
    val tpe: ru.Type = ru.typeOf[List[Int]]
    println(tpe)
  }.eval
}
