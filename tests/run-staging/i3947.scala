
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

    // classOf[Object]
    '{
      ${test(classOf[Object])}
      ${test(classOf[Any])}
      ${test(classOf[AnyRef])}
      ${test(classOf[AnyVal])}
    }
  }

}
