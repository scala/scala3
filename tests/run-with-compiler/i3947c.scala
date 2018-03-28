
import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

object Test {

  def main(args: Array[String]): Unit = {

    def test[T](clazz: java.lang.Class[T]): Unit = {
      val lclazz = clazz.toExpr
      val name = '{ (~lclazz).getCanonicalName }
      println()
      println(name.show)
      println(name.run)
    }

    test(classOf[Null])
    test(classOf[Nothing])

    test(classOf[String])
  }

}
