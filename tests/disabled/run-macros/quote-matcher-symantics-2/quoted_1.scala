import scala.quoted._

import scala.quoted.unsafe._

object Macros {

  inline def liftString(inline a: DSL): String = ${impl(StringNum, 'a)}

  inline def liftCompute(inline a: DSL): Int = ${impl(ComputeNum, 'a)}

  inline def liftAST(inline a: DSL): ASTNum = ${impl(ASTNum, 'a)}

  private def impl[T](using s: Scope)(sym: Symantics[T], a: s.Expr[DSL])(using s.Type[T]): s.Expr[T] = {

    def lift(e: s.Expr[DSL])(implicit env: Map[Int, s.Expr[T]]): s.Expr[T] = e match {

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
        s.tasty.error("Expected explicit DSL " + e.show, e.pos)
        ???
    }

    def liftFun(e: s.Expr[DSL => DSL])(implicit env: Map[Int, s.Expr[T]]): s.Expr[T => T] = e match {
      case '{ (x: DSL) => $bodyFn(x): DSL } =>
        sym.lam((y: s.Expr[T]) =>
          UnsafeExpr.open(bodyFn) { (body1, close) =>
            val (i, nEnvVar) = freshEnvVar()
            lift(close(body1)(nEnvVar))(env + (i -> y))
          }
        )
      case _ =>
        import qctx.tasty._
        error("Expected explicit DSL => DSL "  + e.show, e.pos)
        ???
    }

    lift(a)(Map.empty)
  }

}

def freshEnvVar()(using s: Scope): (Int, s.Expr[DSL]) = {
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
  def value(using s: Scope)(x: Int): s.Expr[Num]
  def plus(using s: Scope)(x: s.Expr[Num], y: s.Expr[Num]): s.Expr[Num]
  def times(using s: Scope)(x: s.Expr[Num], y: s.Expr[Num]): s.Expr[Num]
  def app(using s: Scope)(f: s.Expr[Num => Num], x: s.Expr[Num]): s.Expr[Num]
  def lam(using s: Scope)(body: s.Expr[Num] => Expr[Num]): s.Expr[Num => Num]
}

object StringNum extends Symantics[String] {
  def value(using s: Scope)(x: Int): s.Expr[String] = Expr(x.toString)
  def plus(using s: Scope)(x: s.Expr[String], y: s.Expr[String]): s.Expr[String] = '{ s"${$x} + ${$y}" } // '{ x + " + " + y }
  def times(using s: Scope)(x: s.Expr[String], y: s.Expr[String]): s.Expr[String] = '{ s"${$x} * ${$y}" }
  def app(using s: Scope)(f: s.Expr[String => String], x: s.Expr[String]): s.Expr[String] = Expr.betaReduce('{ $f($x) })
  def lam(using s: Scope)(body: s.Expr[String] => Expr[String]): s.Expr[String => String] = '{ (x: String) => ${body('x)} }
}

object ComputeNum extends Symantics[Int] {
  def value(using s: Scope)(x: Int): s.Expr[Int] = Expr(x)
  def plus(using s: Scope)(x: s.Expr[Int], y: s.Expr[Int]): s.Expr[Int] = '{ $x + $y }
  def times(using s: Scope)(x: s.Expr[Int], y: s.Expr[Int]): s.Expr[Int] = '{ $x * $y }
  def app(using s: Scope)(f: s.Expr[Int => Int], x: s.Expr[Int]): s.Expr[Int] = '{ $f($x) }
  def lam(using s: Scope)(body: s.Expr[Int] => Expr[Int]): s.Expr[Int => Int] = '{ (x: Int) => ${body('x)} }
}

object ASTNum extends Symantics[ASTNum] {
  def value(using s: Scope)(x: Int): s.Expr[ASTNum] = '{ LitAST(${Expr(x)}) }
  def plus(using s: Scope)(x: s.Expr[ASTNum], y: s.Expr[ASTNum]): s.Expr[ASTNum] = '{ PlusAST($x, $y) }
  def times(using s: Scope)(x: s.Expr[ASTNum], y: s.Expr[ASTNum]): s.Expr[ASTNum] = '{ TimesAST($x, $y) }
  def app(using s: Scope)(f: s.Expr[ASTNum => ASTNum], x: s.Expr[ASTNum]): s.Expr[ASTNum] = '{ AppAST($f, $x) }
  def lam(using s: Scope)(body: s.Expr[ASTNum] => Expr[ASTNum]): s.Expr[ASTNum => ASTNum] = '{ (x: ASTNum) => ${body('x)} }
}

trait ASTNum
case class LitAST(x: Int) extends ASTNum
case class PlusAST(x: ASTNum, y: ASTNum) extends ASTNum
case class TimesAST(x: ASTNum, y: ASTNum) extends ASTNum
case class AppAST(x: ASTNum => ASTNum, y: ASTNum) extends ASTNum {
  override def toString: String = s"AppAST(<lambda>, $y)"
}
