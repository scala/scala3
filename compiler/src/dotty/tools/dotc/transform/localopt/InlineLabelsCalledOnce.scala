package dotty.tools.dotc
package transform.localopt

import core.Contexts.Context
import core.Symbols._
import core.Flags._
import transform.SymUtils._
import scala.collection.mutable
import config.Printers.simplify

/** Inlines LabelDef which are used exactly once. */
class InlineLabelsCalledOnce(implicit val ctx: Context) extends Optimisation {
  import ast.tpd._

  val timesUsed = mutable.HashMap[Symbol, Int]()
  val defined   = mutable.HashMap[Symbol, DefDef]()

  val visitor: Tree => Unit = {
    case d: DefDef if d.symbol.is(Label)  =>
      var isRecursive = false
      d.rhs.foreachSubTree { x =>
        if (x.symbol == d.symbol)
          isRecursive = true
      }
      if (!isRecursive)
        defined.put(d.symbol, d)

    case t: Apply if t.symbol.is(Label) =>
      val b4 = timesUsed.getOrElseUpdate(t.symbol, 0)
      timesUsed.put(t.symbol, b4 + 1)

    case _ =>
  }

  def transformer(localCtx: Context): Tree => Tree = {
    case a: Apply =>
      defined.get(a.symbol) match {
        case Some(defDef) if usedOnce(a) && a.symbol.info.paramInfoss == List(Nil) =>
          simplify.println(s"Inlining labeldef ${defDef.name}")
          defDef.rhs.changeOwner(defDef.symbol, localCtx.owner)

        case Some(defDef) if defDef.rhs.isInstanceOf[Literal] =>
          defDef.rhs

        case _ => a
      }

    case d: DefDef if usedOnce(d) =>
      simplify.println(s"Dropping labeldef (used once) ${d.name} ${timesUsed.get(d.symbol)}")
      defined.put(d.symbol, d)
      EmptyTree

    case d: DefDef if neverUsed(d) =>
      simplify.println(s"Dropping labeldef (never used) ${d.name} ${timesUsed.get(d.symbol)}")
      EmptyTree

    case t => t
  }

  def usedN(t: Tree, n: Int): Boolean =
    t.symbol.is(Label)                    &&
    timesUsed.getOrElse(t.symbol, 0) == n &&
    defined.contains(t.symbol)

  def usedOnce(t: Tree): Boolean  = usedN(t, 1)
  def neverUsed(t: Tree): Boolean = usedN(t, 0)
}
