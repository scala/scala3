package dividezero

import dotty.tools.dotc._
import core._
import Contexts.Context
import plugins.{Plugin, PluginPhase}
import Phases.Phase
import ast.tpd
import transform.MegaPhase.MiniPhase
import Decorators._
import Symbols.Symbol
import Constants.Constant
import transform.{LinkAll, Pickler}

class DivideZero extends PluginPhase with Plugin {
  val name: String = "divideZero"
  override val description: String = "divide zero check"

  val phaseName = name

  override val runsAfter = Set(classOf[Pickler])
  override val runsBefore = Set(classOf[LinkAll])

  override def init(options: List[String]): List[PluginPhase] = this :: Nil

  private def isNumericDivide(sym: Symbol)(implicit ctx: Context): Boolean = {
    def test(tpe: String): Boolean =
      (sym.owner eq ctx.requiredClass(tpe.toTermName)) && sym.name.show == "/"

    test("scala.Int") || test("scala.Long") || test("scala.Short") || test("scala.FLoat") || test("scala.Double")
  }

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context): tpd.Tree = tree match {
    case tpd.Apply(fun, tpd.Literal(Constants.Constant(v)) :: Nil) if isNumericDivide(fun.symbol) && v == 0 =>
      ctx.warning("divide by zero", tree.pos)
      tpd.Literal(Constant(0))
    case _ =>
      tree
  }
}
