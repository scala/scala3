object Bar {
  import quoted.*

  inline transparent def bar(): Array[String] = ${ macroBar() }
  def macroBar()(using Quotes): Expr[Array[String]] = {
    val l = Range(0, 200, 10).map(_.toString).map(Expr(_)).toList
    '{ Array[String](${ Varargs(l) }*) }
    //'{ Array[String]("hello", "world") }
  }
}
