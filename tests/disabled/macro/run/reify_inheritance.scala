import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends dotty.runtime.LegacyApp {
  reify {
    class C {
      def x = 2
      def y = x * x
    }

    class D extends C {
      override def x = 3
    }

    println(new D().y * new C().x)
  }.eval
}
