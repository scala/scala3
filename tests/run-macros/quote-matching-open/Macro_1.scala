import scala.quoted._

object Macro {

  inline def openTest(inline x: Any): Any = ${ Macro.impl('x) }

  def impl(x: Expr[Any])(using QuoteContext): Expr[Any] = {
    x match {
      case '{ (x: Int) => $body(x): Int } => UnsafeExpr.open(body) { (body, close) => close(body)(Expr(2)) }
      case '{ (x1: Int, x2: Int) => $body(x1, x2): Int } => UnsafeExpr.open(body) { (body, close) => close(body)(Expr(2), Expr(3)) }
      case '{ (x1: Int, x2: Int, x3: Int) => $body(x1, x2, x3): Int } => UnsafeExpr.open(body) { (body, close) => close(body)(Expr(2), Expr(3), Expr(4)) }
    }
  }

}


object UnsafeExpr {
  def open[T1, R, X](f: Expr[T1 => R])(content: (Expr[R], [t] => Expr[t] => Expr[T1] => Expr[t]) => X)(using qctx: QuoteContext): X = {
    import qctx.reflect._
    val (params, bodyExpr) = paramsAndBody[R](f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v: Expr[T1]) => bodyFn[t](Term.of(e), params, List(Term.of(v))).asExpr.asInstanceOf[Expr[t]])
  }
  def open[T1, T2, R, X](f: Expr[(T1, T2) => R])(content: (Expr[R], [t] => Expr[t] => (Expr[T1], Expr[T2]) => Expr[t]) => X)(using qctx: QuoteContext)(using DummyImplicit): X = {
    import qctx.reflect._
    val (params, bodyExpr) = paramsAndBody[R](f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v1: Expr[T1], v2: Expr[T2]) => bodyFn[t](Term.of(e), params, List(Term.of(v1), Term.of(v2))).asExpr.asInstanceOf[Expr[t]])
  }

  def open[T1, T2, T3, R, X](f: Expr[(T1, T2, T3) => R])(content: (Expr[R], [t] => Expr[t] => (Expr[T1], Expr[T2], Expr[T3]) => Expr[t]) => X)(using qctx: QuoteContext)(using DummyImplicit, DummyImplicit): X = {
    import qctx.reflect._
    val (params, bodyExpr) = paramsAndBody[R](f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v1: Expr[T1], v2: Expr[T2], v3: Expr[T3]) => bodyFn[t](Term.of(e), params, List(Term.of(v1), Term.of(v2), Term.of(v3))).asExpr.asInstanceOf[Expr[t]])
  }
  private def paramsAndBody[R](using qctx: QuoteContext)(f: Expr[Any]): (List[qctx.reflect.ValDef], Expr[R]) = {
    import qctx.reflect._
    val Block(List(DefDef("$anonfun", Nil, List(params), _, Some(body))), Closure(Ident("$anonfun"), None)) = Term.of(f).etaExpand(Symbol.spliceOwner)
    (params, body.asExpr.asInstanceOf[Expr[R]])
  }

  private def bodyFn[t](using qctx: QuoteContext)(e: qctx.reflect.Term, params: List[qctx.reflect.ValDef], args: List[qctx.reflect.Term]): qctx.reflect.Term = {
    import qctx.reflect._
    val map = params.map(_.symbol).zip(args).toMap
    new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        super.transformTerm(tree)(owner) match
          case tree: Ident => map.getOrElse(tree.symbol, tree)
          case tree => tree
    }.transformTerm(e)(Symbol.spliceOwner)
  }
}