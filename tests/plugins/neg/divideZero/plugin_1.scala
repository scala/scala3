import dotty.tools.dotc.*
import core.*
import Contexts.Context
import plugins.*
import Phases.Phase
import ast.tpd
import transform.MegaPhase.MiniPhase
import Decorators.*
import Symbols.{Symbol, requiredClass}
import Constants.Constant
import transform.{Pickler, PickleQuotes}
import StdNames.*

class DivideZero extends PluginPhase with StandardPlugin {
  val name: String = "divideZero"
  override val description: String = "divide zero check"

  val phaseName = name

  override val runsAfter = Set(Pickler.name)
  override val runsBefore = Set(PickleQuotes.name)

  override def initialize(options: List[String])(using Context): List[PluginPhase] = this :: Nil

  private def isNumericDivide(sym: Symbol)(implicit ctx: Context): Boolean = {
    def test(tpe: String): Boolean =
      (sym.owner eq requiredClass(tpe)) && sym.name == nme.DIV

    test("scala.Int") || test("scala.Long") || test("scala.Short") || test("scala.Float") || test("scala.Double")
  }

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context): tpd.Tree = tree match {
    case tpd.Apply(fun, tpd.Literal(Constants.Constant(v)) :: Nil) if isNumericDivide(fun.symbol) && v == 0 =>
      report.error("divide by zero", tree.sourcePos)
      tree
    case _ =>
      tree
  }
}
