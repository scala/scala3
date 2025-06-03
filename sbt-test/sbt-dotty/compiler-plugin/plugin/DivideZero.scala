package dividezero

import dotty.tools.dotc._
import core._
import Contexts.Context
import plugins._
import Phases.Phase
import ast.tpd
import transform.MegaPhase.MiniPhase
import Decorators._
import Symbols._
import Constants.Constant
import transform.{Pickler, Staging}

/** Compiler plugin that emits an error when compiling a division by zero */
class DivideZero extends PluginPhase with StandardPlugin {
  val name: String = "divideZero"
  override val description: String = "divide zero check"

  val phaseName = name

  override val runsAfter = Set(Pickler.name)
  override val runsBefore = Set(Staging.name)

  // We keep using deprecated variant here just to ensure it still works correctly
  override def init(options: List[String]): List[PluginPhase] = this :: Nil

  private def isNumericDivide(sym: Symbol)(implicit ctx: Context): Boolean = {
    def test(tpe: String): Boolean =
      (sym.owner eq requiredClass(tpe)) && sym.name.show == "/"

    test("scala.Int") || test("scala.Long") || test("scala.Short") || test("scala.Float") || test("scala.Double")
  }

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context): tpd.Tree = tree match {
    case tpd.Apply(fun, tpd.Literal(Constants.Constant(v)) :: Nil) if isNumericDivide(fun.symbol) && v == 0 =>
      report.error("divide by zero", tree.sourcePos)
      tpd.Literal(Constant(0))
    case _ =>
      tree
  }
}
