import scala.quoted.*

inline def rewrite[T](inline x: T): T = ${ rewriteMacro('x) }

private def rewriteMacro[T: Type](x: Expr[T])(using Quotes): Expr[T] = {
  val rewriter = Rewriter(
    postTransform = {
      case '{ Nil.map[t]($f) } => '{ Nil }
      case '{ Nil.filter($f) }  => '{ Nil }
      case '{ Nil.++[t]($xs) } => xs
      case '{ ($xs: List[t]).++(Nil) } => xs
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
  def apply(preTransform: Expr[Any] => Expr[Any] = identity, postTransform: Expr[Any] => Expr[Any] = identity, fixPoint: Boolean = false): Rewriter =
    new Rewriter(preTransform, postTransform, fixPoint)
}

private class Rewriter(preTransform: Expr[Any] => Expr[Any], postTransform: Expr[Any] => Expr[Any], fixPoint: Boolean) extends ExprMap {
  def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = {
    val e2 = checkedTransform(e, preTransform)
    val e3 = transformChildren(e2)
    val e4 = checkedTransform(e3, postTransform)
    if fixPoint && !e4.matches(e) then transform(e4)
    else e4
  }

  private def checkedTransform[T: Type](e: Expr[T], transform: Expr[T] => Expr[Any])(using Quotes): Expr[T] = {
    transform(e) match {
      case '{ $x: T } => x
      case '{ $x: t } => throw new Exception(
        s"""Transformed
           |${e.show}
           |into
           |${x.show}
           |
           |Expected type to be
           |${Type.show[T]}
           |but was
           |${Type.show[t]}
         """.stripMargin)
    }
  }

}
