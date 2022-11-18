object M {
  import scala.quoted.*

  transparent inline def valueOfUnit: ValueOf[Unit] =
    ${ _valueOfUnit }: ValueOf[Unit]

  def _valueOfUnit(using Quotes): Expr[ValueOf[Unit]] = {
    import quotes.reflect.*
    Expr.summon[ValueOf[Unit]] getOrElse sys.error("Not found")
  }
}
