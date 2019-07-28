
import scala.quoted._

object Test {

  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = run {
    def test[T: Type](clazz: java.lang.Class[T]) = {
      val lclazz = clazz.toExpr
      val name = '{ ($lclazz).getCanonicalName }
      println()
      println(name.show)
      '{ println($name) }
    }

    // primitives
    '{
      ${test(classOf[Boolean])}
      ${test(classOf[Byte])}
      ${test(classOf[Char])}
    }
  }

}
