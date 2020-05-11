object Macros {
  import scala.quoted._

  inline def go[T](inline t: T) = ${ impl('t) }
  def impl[T](using s: Scope)(expr: s.Expr[T]): s.Expr[Unit] = {
    import s.tasty._

    val tree = expr

    val methods =
      tree.tpe.classSymbol.get.classMethods.map { m =>
        val name = m.show
        m.tree match
          case ddef: DefDef =>
            val returnType = ddef.returnTpt.tpe.show
            s"$name : $returnType"
      }.sorted

    methods.foldLeft[s.Expr[Unit]]('{}) { (res, m) => '{ $res; println(${Expr(m)}) } }
  }
}
