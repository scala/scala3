import scala.language.experimental.macros

object Anno$ extends scala.annotation.StaticAnnotation {
  def apply[T: reflect.ClassTag](x$0: String): T = macro Anno$$.macro$1
}
object Anno$$ {
  def macro$1(c: scala.reflect.macros.blackbox.Context)(x$0: c.Expr[String]): c.Expr[Any] = { // error // error // error
    import c.universe._
    c.Expr(q"""
        println("Hello World!")
      """)
  }
}
@Anno$(s"Hello World!") // error
class MaliciousClass
