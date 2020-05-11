import scala.quoted._
object Test {
  def impl(using s: Scope)(receiver: s.Expr[StringContext]) = {
    import s.tasty.Repeated
    receiver match {
      case '{ StringContext(${Repeated(parts)}: _*) } => // now OK
    }
  }
}
