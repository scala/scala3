object Macros {
  import scala.quoted._

  inline def go[T](inline t: T) = ${ impl('t) }
  def impl[T](expr: Expr[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._

    val tree = Term.of(expr)

    val methods =
      tree.tpe.classSymbol.get.declaredMethods.map { m =>
        val name = m.fullName
        m.tree match
          case ddef: DefDef =>
            val returnType = ddef.returnTpt.tpe.show
            s"$name : $returnType"
      }.sorted

    methods.foldLeft('{}) { (res, m) => '{ $res; println(${Expr(m)}) } }
  }
}
