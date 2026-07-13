import dotty.tools.dotc.*
import core.*
import Contexts.Context
import plugins.*
import ast.tpd
import Decorators.*
import Symbols.{Symbol, requiredClass}
import Constants.Constant
import transform.{Pickler, PickleQuotes}
import StdNames.*

// A minimal compiler plugin used to exercise the two-argument
// `typeCheckErrors`/`typeChecks` overloads, which run plugin phases up to a
// requested phase. The plugin reports an error on any `_ / 0` on a numeric type.
//
// `DivideZeroPlugin` follows the `StandardPlugin` contract by returning a
// freshly constructed phase from `initialize`.
class DivideZeroPlugin extends StandardPlugin {
  val name: String = "divideZeroCheck"
  override val description: String = "divide by zero check"

  override def initialize(options: List[String])(using Context): List[PluginPhase] =
    new DivideZeroPhase :: Nil
}

class DivideZeroPhase extends PluginPhase {
  val phaseName = "divideZeroCheck"

  override val runsAfter = Set(Pickler.name)
  override val runsBefore = Set(PickleQuotes.name)

  private def isNumericDivide(sym: Symbol)(using Context): Boolean = {
    def test(tpe: String): Boolean =
      (sym.owner eq requiredClass(tpe)) && sym.name == nme.DIV
    test("scala.Int") || test("scala.Long") || test("scala.Short") || test("scala.Float") || test("scala.Double")
  }

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = tree match {
    case tpd.Apply(fun, tpd.Literal(Constant(v)) :: Nil) if isNumericDivide(fun.symbol) && v == 0 =>
      report.error("divide by zero", tree.srcPos)
      tree
    case _ =>
      tree
  }
}
