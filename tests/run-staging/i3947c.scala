
import scala.quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = run {
    def test[T](using s: Scope)(clazz: java.lang.Class[T])(using s.Type[T]) = {
      val lclazz = Expr(clazz)
      val name = '{ ($lclazz).getCanonicalName }
      println(name.show)
      '{ println($name) }
    }

    '{
      ${test(classOf[Null])}
      ${test(classOf[Nothing])}

      ${test(classOf[String])}
    }
  }

}
