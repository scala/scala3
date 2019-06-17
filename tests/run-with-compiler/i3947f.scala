
import scala.quoted._

object Test {

  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    def test[T: Type](clazz: java.lang.Class[T]): Unit = {
      val lclazz = clazz.toExpr
      val name = '{ ($lclazz).getCanonicalName }
      println()
      println(name.show)
      println(run(name))
    }

    // class Array[Object]
    test(classOf[Array[Any]])
    test(classOf[Array[AnyVal]])
    test(classOf[Array[AnyRef]])
    test(classOf[Array[Object]])
  }

}

class Foo
