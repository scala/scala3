import scala.quoted._

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {

  implicit inline def line[T >: Unit <: Unit]: LineNumber =
    ${lineImpl('[T])}

  def lineImpl(using s: Scope)(x: s.Type[Unit]): s.Expr[LineNumber] = {
    import s.tasty._
    '{new LineNumber(${Expr(rootPosition.startLine)})}
  }

}
