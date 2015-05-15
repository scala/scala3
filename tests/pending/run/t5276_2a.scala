import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  reify {
    class C {
      lazy val x = 2
    }

    println(new C().x)
  }.eval
}
