
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

    test(classOf[Foo])
    test(classOf[Foo#Bar])
    test(classOf[Foo.Baz])
  }

}

class Foo {
  class Bar
}

object Foo {
  class Baz
}
