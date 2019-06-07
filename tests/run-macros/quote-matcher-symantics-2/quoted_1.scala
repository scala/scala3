import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

object Macros {

  inline def liftString(a: => DSL): String = ${impl(StringNum, 'a)}

  inline def liftCompute(a: => DSL): Int = ${impl(ComputeNum, 'a)}

  inline def liftAST(a: => DSL): ASTNum = ${impl(ASTNum, 'a)}

  private def impl[T: Type](sym: Symantics[T], a: Expr[DSL])(implicit reflect: Reflection): Expr[T] = {

    def lift(e: Expr[DSL])(implicit env: Map[Bind[DSL], Expr[T]]): Expr[T] = e match {

      case '{ LitDSL(${Const(c)}) } => sym.value(c)

      case '{ ($x: DSL) + ($y: DSL) } => sym.plus(lift(x), lift(y))

      case '{ ($x: DSL) * ($y: DSL) } => sym.times(lift(x), lift(y))

      case '{ ($f: DSL => DSL)($x: DSL) } => sym.app(liftFun(f), lift(x))

      case '{ val $x: DSL = $value; $body: DSL } => lift(body)(env + (x -> lift(value)))

      case Bind(b) if env.contains(b) => env(b)

      case _ =>
        import reflect._
        error("Expected explicit DSL", e.unseal.pos)
        ???
    }

    def liftFun(e: Expr[DSL => DSL])(implicit env: Map[Bind[DSL], Expr[T]]): Expr[T => T] = e match {
      case '{ ($x: DSL) => ($body: DSL) } =>
        sym.lam((y: Expr[T]) => lift(body)(env + (x -> y)))

      case _ =>
        import reflect._
        error("Expected explicit DSL => DSL", e.unseal.pos)
        ???
    }

    lift(a)(Map.empty)
  }

}

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
  def value(x: Int): Expr[Num]
  def plus(x: Expr[Num], y: Expr[Num]): Expr[Num]
  def times(x: Expr[Num], y: Expr[Num]): Expr[Num]
  def app(f: Expr[Num => Num], x: Expr[Num]): Expr[Num]
  def lam(body: Expr[Num] => Expr[Num]): Expr[Num => Num]
}

object StringNum extends Symantics[String] {
  def value(x: Int): Expr[String] = x.toString.toExpr
  def plus(x: Expr[String], y: Expr[String]): Expr[String] = '{ s"${$x} + ${$y}" } // '{ x + " + " + y }
  def times(x: Expr[String], y: Expr[String]): Expr[String] = '{ s"${$x} * ${$y}" }
  def app(f: Expr[String => String], x: Expr[String]): Expr[String] = f(x) // functions are beta reduced
  def lam(body: Expr[String] => Expr[String]): Expr[String => String] = '{ (x: String) => ${body('x)} }
}

object ComputeNum extends Symantics[Int] {
  def value(x: Int): Expr[Int] = x.toExpr
  def plus(x: Expr[Int], y: Expr[Int]): Expr[Int] = '{ $x + $y }
  def times(x: Expr[Int], y: Expr[Int]): Expr[Int] = '{ $x * $y }
  def app(f: Expr[Int => Int], x: Expr[Int]): Expr[Int] = '{ $f($x) }
  def lam(body: Expr[Int] => Expr[Int]): Expr[Int => Int] = '{ (x: Int) => ${body('x)} }
}

object ASTNum extends Symantics[ASTNum] {
  def value(x: Int): Expr[ASTNum] = '{ LitAST(${x.toExpr}) }
  def plus(x: Expr[ASTNum], y: Expr[ASTNum]): Expr[ASTNum] = '{ PlusAST($x, $y) }
  def times(x: Expr[ASTNum], y: Expr[ASTNum]): Expr[ASTNum] = '{ TimesAST($x, $y) }
  def app(f: Expr[ASTNum => ASTNum], x: Expr[ASTNum]): Expr[ASTNum] = '{ AppAST($f, $x) }
  def lam(body: Expr[ASTNum] => Expr[ASTNum]): Expr[ASTNum => ASTNum] = '{ (x: ASTNum) => ${body('x)} }
}

trait ASTNum
case class LitAST(x: Int) extends ASTNum
case class PlusAST(x: ASTNum, y: ASTNum) extends ASTNum
case class TimesAST(x: ASTNum, y: ASTNum) extends ASTNum
case class AppAST(x: ASTNum => ASTNum, y: ASTNum) extends ASTNum {
  override def toString: String = s"AppAST(<lambda>, $y)"
}
