import scala.quoted.*

final case class caseName(name: String) extends scala.annotation.Annotation
object caseName {
  // This demonstrates a workaround for issue #22616.
  given FromExpr[caseName] =
    new FromExpr[caseName] {
      override def unapply(x: Expr[caseName])(using Quotes): Option[caseName] =
        x match {
          case '{ new `caseName`(${ Expr(name) }) } => Some(caseName(name))
          case _                                    => println(x.show); None
        }
    }
}
