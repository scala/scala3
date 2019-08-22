
import scala.quoted._
import scala.quoted.staging._

object Test {

  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = run {
    def test[T: Type](clazz: given QuoteContext => java.lang.Class[T]) = {
      val lclazz = clazz.toExpr
      val name = '{ ($lclazz).getCanonicalName }
      println()
      println(name.show)
      '{ println($name) }
    }

    // primitives
    '{
      ${test(classOf[Short])}
      ${test(classOf[Int])}
      ${test(classOf[Long])}
    }
  }

}
