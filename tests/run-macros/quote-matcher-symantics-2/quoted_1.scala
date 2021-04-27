import scala.quoted.*

object Macros {

  inline def liftString(inline a: DSL): String = ${implStringNum('a)}

  private def implStringNum(a: Expr[DSL])(using Quotes): Expr[String] =
    impl(StringNum, a)

  inline def liftCompute(inline a: DSL): Int = ${implComputeNum('a)}

  private def implComputeNum(a: Expr[DSL])(using Quotes): Expr[Int] =
    impl(ComputeNum, a)

  inline def liftAST(inline a: DSL): ASTNum = ${implASTNum('a)}

  private def implASTNum(a: Expr[DSL])(using Quotes): Expr[ASTNum] =
    impl(ASTNum, a)

  private def impl[T: Type](sym: Symantics[T], a: Expr[DSL])(using Quotes): Expr[T] = {

    def lift(e: Expr[DSL])(using env: Map[Int, Expr[T]])(using Quotes): Expr[T] = e match {

      case '{ LitDSL(${Expr(c)}) } => sym.value(c)

      case '{ ($x: DSL) + ($y: DSL) } => sym.plus(lift(x), lift(y))

      case '{ ($x: DSL) * ($y: DSL) } => sym.times(lift(x), lift(y))

      case '{ ${f}($x: DSL): DSL } => sym.app(liftFun(f), lift(x))

      case '{ val x: DSL = $value; $bodyFn(x): DSL } =>
        UnsafeExpr.open(bodyFn) { (body1, close) =>
          val (i, nEnvVar) = freshEnvVar()
          lift(close(body1)(nEnvVar))(using env + (i -> lift(value)))
        }

      case '{ envVar(${Expr(i)}) } => env(i)

      case _ =>
        import quotes.reflect.*
        report.error("Expected explicit DSL " + e.show, e.asTerm.pos)
        ???
    }

    def liftFun(e: Expr[DSL => DSL])(using env: Map[Int, Expr[T]])(using Quotes): Expr[T => T] = e match {
      case '{ (x: DSL) => $bodyFn(x): DSL } =>
        sym.lam((y: Expr[T]) =>
          UnsafeExpr.open(bodyFn) { (body1, close) =>
            val (i, nEnvVar) = freshEnvVar()
            lift(close(body1)(nEnvVar))(using env + (i -> y))
          }
        )
      case _ =>
        import quotes.reflect.*
        report.error("Expected explicit DSL => DSL "  + e.show, e.asTerm.pos)
        ???
    }

    lift(a)(using Map.empty)
  }

}

object UnsafeExpr {
  def open[T1, R, X](f: Expr[T1 => R])(content: (Expr[R], [t] => Expr[t] => Expr[T1] => Expr[t]) => X)(using Quotes): X = {
    import quotes.reflect.*
    val (params, bodyExpr) = paramsAndBody[R](f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v: Expr[T1]) => bodyFn[t](e.asTerm, params, List(v.asTerm)).asExpr.asInstanceOf[Expr[t]])
  }
  private def paramsAndBody[R](using Quotes)(f: Expr[Any]): (List[quotes.reflect.ValDef], Expr[R]) = {
    import quotes.reflect.*
    val Block(List(DefDef("$anonfun", List(TermParamClause(params)), _, Some(body))), Closure(Ident("$anonfun"), None)) = f.asTerm.etaExpand(Symbol.spliceOwner)
    (params, body.asExpr.asInstanceOf[Expr[R]])
  }

  private def bodyFn[t](using Quotes)(e: quotes.reflect.Term, params: List[quotes.reflect.ValDef], args: List[quotes.reflect.Term]): quotes.reflect.Term = {
    import quotes.reflect.*
    val map = params.map(_.symbol).zip(args).toMap
    new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        super.transformTerm(tree)(owner) match
          case tree: Ident => map.getOrElse(tree.symbol, tree)
          case tree => tree
    }.transformTerm(e)(Symbol.spliceOwner)
  }
}

def freshEnvVar()(using Quotes): (Int, Expr[DSL]) = {
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
  def value(x: Int)(using Quotes): Expr[Num]
  def plus(x: Expr[Num], y: Expr[Num])(using Quotes): Expr[Num]
  def times(x: Expr[Num], y: Expr[Num])(using Quotes): Expr[Num]
  def app(f: Expr[Num => Num], x: Expr[Num])(using Quotes): Expr[Num]
  def lam(body: Quotes ?=> Expr[Num] => Expr[Num])(using Quotes): Expr[Num => Num]
}

object StringNum extends Symantics[String] {
  def value(x: Int)(using Quotes): Expr[String] = Expr(x.toString)
  def plus(x: Expr[String], y: Expr[String])(using Quotes): Expr[String] = '{ s"${$x} + ${$y}" } // '{ x + " + " + y }
  def times(x: Expr[String], y: Expr[String])(using Quotes): Expr[String] = '{ s"${$x} * ${$y}" }
  def app(f: Expr[String => String], x: Expr[String])(using Quotes): Expr[String] = Expr.betaReduce('{ $f($x) })
  def lam(body: Quotes ?=> Expr[String] => Expr[String])(using Quotes): Expr[String => String] = '{ (x: String) => ${body('x)} }
}

object ComputeNum extends Symantics[Int] {
  def value(x: Int)(using Quotes): Expr[Int] = Expr(x)
  def plus(x: Expr[Int], y: Expr[Int])(using Quotes): Expr[Int] = '{ $x + $y }
  def times(x: Expr[Int], y: Expr[Int])(using Quotes): Expr[Int] = '{ $x * $y }
  def app(f: Expr[Int => Int], x: Expr[Int])(using Quotes): Expr[Int] = '{ $f($x) }
  def lam(body: Quotes ?=> Expr[Int] => Expr[Int])(using Quotes): Expr[Int => Int] = '{ (x: Int) => ${body('x)} }
}

object ASTNum extends Symantics[ASTNum] {
  def value(x: Int)(using Quotes): Expr[ASTNum] = '{ LitAST(${Expr(x)}) }
  def plus(x: Expr[ASTNum], y: Expr[ASTNum])(using Quotes): Expr[ASTNum] = '{ PlusAST($x, $y) }
  def times(x: Expr[ASTNum], y: Expr[ASTNum])(using Quotes): Expr[ASTNum] = '{ TimesAST($x, $y) }
  def app(f: Expr[ASTNum => ASTNum], x: Expr[ASTNum])(using Quotes): Expr[ASTNum] = '{ AppAST($f, $x) }
  def lam(body: Quotes ?=> Expr[ASTNum] => Expr[ASTNum])(using Quotes): Expr[ASTNum => ASTNum] = '{ (x: ASTNum) => ${body('x)} }
}

trait ASTNum
case class LitAST(x: Int) extends ASTNum
case class PlusAST(x: ASTNum, y: ASTNum) extends ASTNum
case class TimesAST(x: ASTNum, y: ASTNum) extends ASTNum
case class AppAST(x: ASTNum => ASTNum, y: ASTNum) extends ASTNum {
  override def toString: String = s"AppAST(<lambda>, $y)"
}
