
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

    '{
      ${test(classOf[foo.Foo])}
      ${test(classOf[foo.Foo#Bar])}
      ${test(classOf[foo.Foo.Baz])}
    }
  }

}

package foo {
  class Foo {
    class Bar
  }
  object Foo {
    class Baz
  }
}
