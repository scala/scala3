import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  reify {
    2 match {
      case 2 => println("okay")
      case _ => println("not okay")
    }
  }.eval
}
