
import scala.quoted._

object Test {

  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make(getClass.getClassLoader)
    def test[T: Type](clazz: java.lang.Class[T]): Unit = {
      def lclazz: Staged[Class[T]] = clazz.toExpr
      def name: Staged[String] = '{ ($lclazz).getCanonicalName }
      println()
      println(tb.run(name.show.toExpr))
      println(tb.run(name))
    }

    // class Array[Object]
    test(classOf[Array[Any]])
    test(classOf[Array[AnyVal]])
    test(classOf[Array[AnyRef]])
    test(classOf[Array[Object]])
  }

}

class Foo
