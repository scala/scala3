package scala.quoted

class QuoteContext(reflection: tasty.Reflection) {

  def show[T](expr: Expr[T], formatted: Boolean): String = {
    import reflection._
    if (formatted) expr.unseal.showFormatted
    else expr.unseal.show
  }

  def show[T](tpe: Type[T], formatted: Boolean): String = {
    import reflection._
    if (formatted) tpe.unseal.showFormatted
    else tpe.unseal.show
  }

}
