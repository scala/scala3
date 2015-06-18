import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  println(reify{ru}.eval.getClass)
}
