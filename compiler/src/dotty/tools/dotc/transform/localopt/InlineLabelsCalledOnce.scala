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
  val defined = mutable.HashMap[Symbol, DefDef]()

  val visitor: Tree => Unit = {
    case defdef: DefDef if defdef.symbol.is(Label)  =>
      var isRecursive = false
      defdef.rhs.foreachSubTree(x => if (x.symbol == defdef.symbol) isRecursive = true)
      if (!isRecursive) defined.put(defdef.symbol, defdef)
    case t: Apply if t.symbol.is(Label) =>
      val b4 = timesUsed.getOrElseUpdate(t.symbol, 0)
      timesUsed.put(t.symbol, b4 + 1)
    case _ =>
  }

  def transformer(localCtx: Context): Tree => Tree = {
    case a: Apply =>
      defined.get(a.symbol) match {
        case None => a
        case Some(defDef) if a.symbol.is(Label) && timesUsed.getOrElse(a.symbol, 0) == 1 && a.symbol.info.paramInfoss == List(Nil) =>
          simplify.println(s"Inlining labeldef ${defDef.name}")
          defDef.rhs.changeOwner(defDef.symbol, localCtx.owner)
        case Some(defDef) if defDef.rhs.isInstanceOf[Literal] =>
          defDef.rhs
        case Some(_) =>
          a
      }
    case a: DefDef if (a.symbol.is(Label) && timesUsed.getOrElse(a.symbol, 0) == 1 && defined.contains(a.symbol)) =>
      simplify.println(s"Dropping labeldef (used once) ${a.name} ${timesUsed.get(a.symbol)}")
      defined.put(a.symbol, a)
      EmptyTree
    case a: DefDef if (a.symbol.is(Label) && timesUsed.getOrElse(a.symbol, 0) == 0 && defined.contains(a.symbol)) =>
      simplify.println(s"Dropping labeldef (never used) ${a.name} ${timesUsed.get(a.symbol)}")
      EmptyTree
    case t => t
  }
}
