
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

    '{
      // class Object
      ${test(classOf[Array[_]])}

      // class Array[Foo]
      ${test(classOf[Array[Foo]])}

      // class Array[Array[Foo]]
      ${test(classOf[Array[Array[Foo]]])}
    }
  }

}

class Foo
