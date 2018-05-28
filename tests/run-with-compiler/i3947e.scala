
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

    // class Object
    test(classOf[Array[_]])

    // class Array[Foo]
    test(classOf[Array[Foo]])

    // class Array[Array[Foo]]
    test(classOf[Array[Array[Foo]]])
  }

}

class Foo
