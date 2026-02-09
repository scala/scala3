import scala.quoted.*

final case class caseName(name: String) extends scala.annotation.Annotation
object caseName {

  given FromExpr[caseName] =
    new FromExpr[caseName] {
      override def unapply(x: Expr[caseName])(using Quotes): Option[caseName] =
        val y: Int = 42
        x match {
          case '{ caseName(${ Expr(name) }) }     => Some(caseName(name))
          // with/without the following line...
          case '{ new caseName(${ Expr(name) }) } => Some(caseName(name))  // error // error
          case _                                  => println(x.show); None
        }
    }

}
