import scala.quoted._

inline def rewrite[T](inline x: T): T = ${ rewriteMacro('x) }

private def rewriteMacro[T](using s: Scope)(x: s.Expr[T])(using s.Type[T]): s.Expr[T] = {
  val rewriter = Rewriter(
    postTransform = (s: Scope) => (x: s.Expr[Any]) =>
      given s.type = s // FIXME improve API to have `s` as an implicit
      x match {
      case '{ Nil.map[$t]($f) } => '{ Nil }
      case '{ Nil.filter($f) }  => '{ Nil }
      case '{ Nil.++[$t]($xs) } => xs
      case '{ ($xs: List[$t]).++(Nil) } => xs
      case x => x
    }
  )

  val x2 = rewriter.transform(x)

  '{
    println(${Expr(x.show)})
    println(${Expr(x2.show)})
    println()
    $x2
  }
}

private object Rewriter {
  def apply(preTransform: (s: Scope) => s.Expr[Any] => s.Expr[Any] = default, postTransform: (s: Scope) => s.Expr[Any] => s.Expr[Any] = default, fixPoint: Boolean = false): Rewriter =
    new Rewriter(preTransform, postTransform, fixPoint)

 def default: (s: Scope) => s.Expr[Any] => s.Expr[Any] =
  (s: Scope) => (x: s.Expr[Any]) => x
}

private class Rewriter(preTransform: (s: Scope) => s.Expr[Any] => s.Expr[Any], postTransform: (s: Scope) => s.Expr[Any] => s.Expr[Any], fixPoint: Boolean) extends util.ExprMap {
  def transform[T](using s: Scope)(e: s.Expr[T])(using s.Type[T]): s.Expr[T] = {
    val e2 = checkedTransform(e, preTransform(s))
    val e3 = transformChildren(e2)
    val e4 = checkedTransform(e3, postTransform(s))
    if fixPoint && !e4.matches(e) then transform(e4)
    else e4
  }

  private def checkedTransform[T](using s: Scope)(e: s.Expr[T], transform: s.Expr[T] => s.Expr[Any])(using s.Type[T]): s.Expr[T] = {
    transform(e) match {
      case '{ $x: T } => x
      case '{ $x: $t } => throw new Exception(
        s"""Transformed
           |${e.show}
           |into
           |${x.show}
           |
           |Expected type to be
           |${summon[s.Type[T]].asInstanceOf[s.Type[T]].show}
           |but was
           |${t.asInstanceOf[s.Type[Any]].show}
         """.stripMargin)
    }
  }

}
