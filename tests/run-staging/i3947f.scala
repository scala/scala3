
import scala.quoted._
import scala.quoted.staging._

object Test {

  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = run {
    def test[T: Type](clazz: java.lang.Class[T]) = {
      val lclazz = clazz.toExpr
      val name = '{ ($lclazz).getCanonicalName }
      println(name.show)
      '{ println($name) }
    }

    // class Array[Object]
    '{
      ${test(classOf[Array[Any]])}
      ${test(classOf[Array[AnyVal]])}
      ${test(classOf[Array[AnyRef]])}
      ${test(classOf[Array[Object]])}
    }
  }

}

class Foo
