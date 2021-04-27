
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
      ${test(classOf[Foo])}
      ${test(classOf[Foo#Bar])}
      ${test(classOf[Foo.Baz])}
    }
  }

}

class Foo {
  class Bar
}

object Foo {
  class Baz
}
