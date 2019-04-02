
import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

import scala.runtime.quoted.Matcher._

object Macros {

  inline def liftString(a: => DSL): String = ${impl(StringNum, 'a)}

  inline def liftCompute(a: => DSL): Int = ${impl(ComputeNum, 'a)}

  inline def liftAST(a: => DSL): ASTNum = ${impl(ASTNum, 'a)}

  private def impl[T: Type](sym: Symantics[T], a: Expr[DSL])(implicit reflect: Reflection): Expr[T] = {

    object ValueExpr extends ExprMatch[Tuple1[Expr[Int]]]('{LitDSL(hole[Int])}) // case '{ LitDSL($lit) } =>
    object PlusExpr extends ExprMatch[Tuple2[Expr[DSL], Expr[DSL]]]('{hole[DSL] + hole[DSL]}) // case '{ ($x: DSL) + ($y: DSL) } =>
    object TimesExpr extends ExprMatch[Tuple2[Expr[DSL], Expr[DSL]]]('{hole[DSL] * hole[DSL]}) // case '{ ($x: DSL) * ($y: DSL) } =>
    object AppExpr extends ExprMatch[Tuple2[Expr[DSL => DSL], Expr[DSL]]]('{ (hole[DSL => DSL]).apply(hole[DSL]) }) // case '{ ($f: DSL => DSL)($x: DSL) } =>
    object LambdaExpr extends ExprMatch[Tuple2[Binding[DSL], Expr[DSL]]]('{ (x: bindHole[DSL]) => hole[DSL] }) // case '{ ($x: DSL) => ($f: DSL) } =>
    object LetExpr extends ExprMatch[Tuple3[Binding[DSL], Expr[DSL], Expr[DSL]]]('{ val x: bindHole[DSL] = hole[DSL]; hole[DSL] }) // case '{ val $x: DSL = $value; $body: DSL } =>

    def lift(e: Expr[DSL])(implicit env: Map[Binding[DSL], Expr[T]]): Expr[T] = e match {

      // case '{ LitDSL(${Literal(c)}) } =>
      // case scala.runtime.quoted.Matcher.unapply[Tuple1[Expr[Int]]](Tuple1(Literal(c: Int)))(/*implicits*/ '{ LitDSL(hole[Int]) }, reflect) =>
      case ValueExpr(Tuple1(Literal(c))) =>
        sym.value(c)

      // case '{ ($x: DSL) + ($y: DSL) } =>
      // case scala.runtime.quoted.Matcher.unapply[Tuple2[Expr[DSL], Expr[DSL]]](Tuple2(x, y))(/*implicits*/ '{ (hole: DSL) + (hole: DSL) }, reflect) =>
      case PlusExpr(Tuple2(x, y)) =>
        sym.plus(lift(x), lift(y))

      // case '{ ($x: DSL) * ($y: DSL) } =>
      // case scala.runtime.quoted.Matcher.unapply[Tuple2[Expr[DSL], Expr[DSL]]](Tuple2(x, y))(/*implicits*/ '{ (hole: DSL) * (hole: DSL) }, reflect) =>
      case TimesExpr(Tuple2(x, y)) =>
        sym.times(lift(x), lift(y))

      // case '{ ($f: DSL => DSL)($x: DSL) } =>
      // case scala.runtime.quoted.Matcher.unapply[Tuple2[Expr[DSL => DSL], Expr[DSL]]](Tuple2(f, x))(/*implicits*/ '{ (hole[DSL => DSL])(hole[DSL]) }, reflect) =>
      case AppExpr(Tuple2(f, x)) =>
        sym.app(liftFun(f), lift(x))

      // case '{ val $x: DSL = $value; $body: DSL } =>
      // case scala.runtime.quoted.Matcher.unapply[Tuple2[Binding[DSL], Expr[DSL]]](Tuple2(f, x))(/*implicits*/ '{ val x: bindHole[DSL] = hole[DSL]; hole[DSL] }, reflect) =>
      case LetExpr(Tuple3(binding, value, rest)) =>
        // TODO push environment to Symantics: define `let` and `ref`
        val liftedValue = lift(value)
        lift(rest)(env + (binding -> liftedValue))

      case Binding(b) if env.contains(b) =>
        env(b)

      case _ =>
        import reflect._
        error("Expected explicit DSL", e.unseal.pos)
        ???

    }

    def liftFun(e: Expr[DSL => DSL])(implicit env: Map[Binding[DSL], Expr[T]]): Expr[T => T] = e match {
      // case '{ ($binding: DSL) => ($body: DSL) } =>
      // case scala.runtime.quoted.Matcher.unapply[Tuple3[Binding[DSL], Expr[DSL], Expr[DSL]]](Tuple2(x, body))(/*implicits*/ '{ (x: bindHole[DSL]) => hole[DSL] }, reflect) =>
      case LambdaExpr(Tuple2(binding, body)) =>
        sym.lam((x: Expr[T]) => lift(body)(env + (binding -> x)))

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

//
// Helper to abstract call to scala.runtime.quoted.Matcher.unapply and setup an object with the unapply
//

class ExprMatch[Tup <: Tuple](pattern: Expr[_]) {
  def unapply(x: Expr[_])(implicit reflect: Reflection): Option[Tup] =
    scala.runtime.quoted.Matcher.unapply[Tup](x)(pattern, reflect)
}
