import scala.quoted._

object Macros {

  inline def liftString(inline a: DSL): String = ${implStringNum('a)}

  private def implStringNum(a: Expr[DSL])(using qctx: QuoteContext): Expr[String] =
    impl(StringNum, a)

  inline def liftCompute(inline a: DSL): Int = ${implComputeNum('a)}

  private def implComputeNum(a: Expr[DSL])(using qctx: QuoteContext): Expr[Int] =
    impl(ComputeNum, a)

  inline def liftAST(inline a: DSL): ASTNum = ${implASTNum('a)}

  private def implASTNum(a: Expr[DSL])(using qctx: QuoteContext): Expr[ASTNum] =
    impl(ASTNum, a)

  private def impl[T: Type](sym: Symantics[T], a: Expr[DSL])(using qctx: QuoteContext): Expr[T] = {

    def lift(e: Expr[DSL])(implicit env: Map[Int, Expr[T]]): Expr[T] = e match {

      case '{ LitDSL(${Const(c)}) } => sym.value(c)

      case '{ ($x: DSL) + ($y: DSL) } => sym.plus(lift(x), lift(y))

      case '{ ($x: DSL) * ($y: DSL) } => sym.times(lift(x), lift(y))

      case '{ ${f}($x: DSL): DSL } => sym.app(liftFun(f), lift(x))

      case '{ val x: DSL = $value; $bodyFn(x): DSL } =>
        UnsafeExpr.open(bodyFn) { (body1, close) =>
          val (i, nEnvVar) = freshEnvVar()
          lift(close(body1)(nEnvVar))(env + (i -> lift(value)))
        }

      case '{ envVar(${Const(i)}) } => env(i)

      case _ =>
        import qctx.reflect._
        Reporting.error("Expected explicit DSL " + e.show, Term.of(e).pos)
        ???
    }

    def liftFun(e: Expr[DSL => DSL])(implicit env: Map[Int, Expr[T]]): Expr[T => T] = e match {
      case '{ (x: DSL) => $bodyFn(x): DSL } =>
        sym.lam((y: Expr[T]) =>
          UnsafeExpr.open(bodyFn) { (body1, close) =>
            val (i, nEnvVar) = freshEnvVar()
            lift(close(body1)(nEnvVar))(env + (i -> y))
          }
        )
      case _ =>
        import qctx.reflect._
        Reporting.error("Expected explicit DSL => DSL "  + e.show, Term.of(e).pos)
        ???
    }

    lift(a)(Map.empty)
  }

}

object UnsafeExpr {
  def open[T1, R, X](f: Expr[T1 => R])(content: (Expr[R], [t] => Expr[t] => Expr[T1] => Expr[t]) => X)(using qctx: QuoteContext): X = {
    import qctx.reflect._
    val (params, bodyExpr) = paramsAndBody[R](f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v: Expr[T1]) => bodyFn[t](Term.of(e), params, List(Term.of(v))).asExpr.asInstanceOf[Expr[t]])
  }
  private def paramsAndBody[R](using qctx: QuoteContext)(f: Expr[Any]): (List[qctx.reflect.ValDef], Expr[R]) = {
    import qctx.reflect._
    val Block(List(DefDef("$anonfun", Nil, List(params), _, Some(body))), Closure(Ident("$anonfun"), None)) = Term.of(f).etaExpand(Symbol.currentOwner)
    (params, body.asExpr.asInstanceOf[Expr[R]])
  }

  private def bodyFn[t](using qctx: QuoteContext)(e: qctx.reflect.Term, params: List[qctx.reflect.ValDef], args: List[qctx.reflect.Term]): qctx.reflect.Term = {
    import qctx.reflect._
    val map = params.map(_.symbol).zip(args).toMap
    new TreeMap {
      override def transformTerm(tree: Term)(using ctx: Context): Term =
        super.transformTerm(tree) match
          case tree: Ident => map.getOrElse(tree.symbol, tree)
          case tree => tree
    }.transformTerm(e)
  }
}

def freshEnvVar()(using QuoteContext): (Int, Expr[DSL]) = {
  v += 1
  (v, '{envVar(${Expr(v)})})
}
var v = 0
def envVar(i: Int): DSL = ???

//
// DSL in which the user write the code
//

trait DSL {
  def + (x: DSL): DSL = ???
  def * (x: DSL): DSL = ???
}
case class LitDSL(x: Int) extends DSL

//
// Interpretation of the DSL
//

trait Symantics[Num] {
  def value(x: Int)(using QuoteContext): Expr[Num]
  def plus(x: Expr[Num], y: Expr[Num])(using QuoteContext): Expr[Num]
  def times(x: Expr[Num], y: Expr[Num])(using QuoteContext): Expr[Num]
  def app(f: Expr[Num => Num], x: Expr[Num])(using QuoteContext): Expr[Num]
  def lam(body: Expr[Num] => Expr[Num])(using QuoteContext): Expr[Num => Num]
}

object StringNum extends Symantics[String] {
  def value(x: Int)(using QuoteContext): Expr[String] = Expr(x.toString)
  def plus(x: Expr[String], y: Expr[String])(using QuoteContext): Expr[String] = '{ s"${$x} + ${$y}" } // '{ x + " + " + y }
  def times(x: Expr[String], y: Expr[String])(using QuoteContext): Expr[String] = '{ s"${$x} * ${$y}" }
  def app(f: Expr[String => String], x: Expr[String])(using QuoteContext): Expr[String] = Expr.betaReduce('{ $f($x) })
  def lam(body: Expr[String] => Expr[String])(using QuoteContext): Expr[String => String] = '{ (x: String) => ${body('x)} }
}

object ComputeNum extends Symantics[Int] {
  def value(x: Int)(using QuoteContext): Expr[Int] = Expr(x)
  def plus(x: Expr[Int], y: Expr[Int])(using QuoteContext): Expr[Int] = '{ $x + $y }
  def times(x: Expr[Int], y: Expr[Int])(using QuoteContext): Expr[Int] = '{ $x * $y }
  def app(f: Expr[Int => Int], x: Expr[Int])(using QuoteContext): Expr[Int] = '{ $f($x) }
  def lam(body: Expr[Int] => Expr[Int])(using QuoteContext): Expr[Int => Int] = '{ (x: Int) => ${body('x)} }
}

object ASTNum extends Symantics[ASTNum] {
  def value(x: Int)(using QuoteContext): Expr[ASTNum] = '{ LitAST(${Expr(x)}) }
  def plus(x: Expr[ASTNum], y: Expr[ASTNum])(using QuoteContext): Expr[ASTNum] = '{ PlusAST($x, $y) }
  def times(x: Expr[ASTNum], y: Expr[ASTNum])(using QuoteContext): Expr[ASTNum] = '{ TimesAST($x, $y) }
  def app(f: Expr[ASTNum => ASTNum], x: Expr[ASTNum])(using QuoteContext): Expr[ASTNum] = '{ AppAST($f, $x) }
  def lam(body: Expr[ASTNum] => Expr[ASTNum])(using QuoteContext): Expr[ASTNum => ASTNum] = '{ (x: ASTNum) => ${body('x)} }
}

trait ASTNum
case class LitAST(x: Int) extends ASTNum
case class PlusAST(x: ASTNum, y: ASTNum) extends ASTNum
case class TimesAST(x: ASTNum, y: ASTNum) extends ASTNum
case class AppAST(x: ASTNum => ASTNum, y: ASTNum) extends ASTNum {
  override def toString: String = s"AppAST(<lambda>, $y)"
}
