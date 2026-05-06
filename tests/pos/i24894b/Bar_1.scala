object Bar {
  import quoted.*

  transparent inline def bar(): String = ${ macroBar() }
  def macroBar()(using Quotes): Expr[String] = {
    val s = "hello".reverse
    Expr(s)
  }
}
