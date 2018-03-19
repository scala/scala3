import dotty.tools.dotc._
import core._
import Contexts.Context
import plugins.Plugin
import Phases.Phase
import ast.tpd
import transform.MegaPhase.MiniPhase
import Decorators._
import Symbols.Symbol
import Constants.Constant
import StdNames._


class DivideZero extends MiniPhase with Plugin {
  val name: String = "divideZero"
  override val description: String = "divide by zero check"

  val phaseName = name

  override val research = true

  override def init(options: List[String], phases: List[List[Phase]])(implicit ctx: Context): List[List[Phase]] = {
    val (before, after) = phases.span(ps => !ps.exists(_.phaseName == "pickler"))
    before ++ (List(this) :: after)
  }

  private def isNumericDivide(sym: Symbol)(implicit ctx: Context): Boolean = {
    def test(tpe: String): Boolean =
      (sym.owner eq ctx.requiredClass(tpe.toTermName)) && sym.name == nme.DIV

    test("scala.Int") || test("scala.Long") || test("scala.Short") || test("scala.Float") || test("scala.Double")
  }

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context): tpd.Tree = tree match {
    case tpd.Apply(fun, tpd.Literal(Constants.Constant(v)) :: Nil) if isNumericDivide(fun.symbol) && v == 0 =>
      ctx.warning("divide by zero", tree.pos)
      tree
    case _ =>
      tree
  }
}
