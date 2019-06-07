import scala.quoted._
object Test {
  def impl(receiver: Expr[StringContext])(implicit reflect: scala.tasty.Reflection) = {
    import reflect.Repeated
    receiver match {
      case '{ StringContext(${Repeated(parts)}: _*) } => // error
    }
  }
}
