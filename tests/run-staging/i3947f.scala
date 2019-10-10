
import scala.quoted._
import scala.quoted.staging._

object Test {

  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = run {
    def test[T: TypeTag](clazz: java.lang.Class[T]) = {
      val lclazz = Expr(clazz)
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
