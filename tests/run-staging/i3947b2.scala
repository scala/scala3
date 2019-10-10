
import scala.quoted._
import scala.quoted.staging._

object Test {

  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = run {
    def test[T: TypeTag](clazz: (given QuoteContext) => java.lang.Class[T]) = {
      val lclazz = Expr(clazz)
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
