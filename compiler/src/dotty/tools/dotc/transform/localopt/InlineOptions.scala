package dotty.tools.dotc
package transform.localopt

import core.Constants.Constant
import core.Contexts.Context
import core.StdNames._
import core.Symbols._
import core.Flags._
import ast.Trees._
import scala.collection.mutable

/** Inlines Option methods whose result is known statically.
 *
 *
 * @author DarkDimius, OlivierBlanvillain
 * */
class InlineOptions extends Optimisation {
  import ast.tpd._

  val somes = mutable.HashMap[Symbol, Tree]()
  val nones = mutable.HashSet[Symbol]()

  def clear(): Unit = {
    somes.clear()
    nones.clear()
  }

  def visitor(implicit ctx: Context): Tree => Unit = {
    case valdef: ValDef if !valdef.symbol.is(Mutable) &&
      valdef.rhs.isInstanceOf[Apply] && valdef.rhs.tpe.derivesFrom(defn.SomeClass) &&
      valdef.rhs.symbol.isPrimaryConstructor =>
      val Apply(_, value) = valdef.rhs
      somes(valdef.symbol) = value.head

    case valdef: ValDef if !valdef.symbol.is(Mutable) &&
      valdef.rhs.isInstanceOf[Apply] && valdef.rhs.tpe.derivesFrom(defn.NoneClass)  =>
      nones += valdef.symbol
    case _ =>
  }

  def transformer(implicit ctx: Context): Tree => Tree = { tree =>
    def rewriteSelect(x: Tree) = x match {
      case Select(rec, nm) if nm == nme.get       && somes.contains(rec.symbol) => somes(rec.symbol)
      case Select(rec, nm) if nm == nme.isDefined && somes.contains(rec.symbol) => Literal(Constant(true))
      case Select(rec, nm) if nm == nme.isEmpty   && somes.contains(rec.symbol) => Literal(Constant(false))
      case Select(rec, nm) if nm == nme.get       && nones.contains(rec.symbol) => ref(defn.NoneModuleRef)
      case Select(rec, nm) if nm == nme.isDefined && nones.contains(rec.symbol) => Literal(Constant(false))
      case Select(rec, nm) if nm == nme.isEmpty   && nones.contains(rec.symbol) => Literal(Constant(true))
      case t => t
    }
    def dropApply(a: Tree): Tree = a match {
      case Apply(fun, Nil) => fun
      case _ => a
    }
    val old = dropApply(tree)
    val nw = rewriteSelect(old)
    if (nw ne old) nw
    else tree
  }
}
