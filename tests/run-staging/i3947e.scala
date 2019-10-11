
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
