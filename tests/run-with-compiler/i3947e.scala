
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

    // class Object
    test(classOf[Array[_]])

    // class Array[Foo]
    test(classOf[Array[Foo]])

    // class Array[Array[Foo]]
    test(classOf[Array[Array[Foo]]])
  }

}

class Foo
