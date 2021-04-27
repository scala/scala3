
import scala.quoted.*
import scala.quoted.staging.*

object Test {

  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = run {
    def test[T: Type](clazz: java.lang.Class[T])(using Quotes) = {
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
