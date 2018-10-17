
import scala.quoted._

object Test {

  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)
    def test[T: Type](clazz: java.lang.Class[T]): Unit = {
      def lclazz: Staged[Class[T]] = clazz.toExpr
      def name: Staged[String] = '{ ($lclazz).getCanonicalName }
      println()
      println(tb.show(name))
      println(tb.run(name))
    }

    test(classOf[foo.Foo])
    test(classOf[foo.Foo#Bar])
    test(classOf[foo.Foo.Baz])
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
