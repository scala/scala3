package dotty.tools.dotc
package transform.localopt

import core._
import core.Contexts.Context
import core.Symbols._
import core.Flags._
import scala.collection.mutable

/** Inline val with exactly one assignment to a var. For example:
 *
 *  {
 *    val l = <expr>
 *    var r = l
 *    // code not using l
 *  }
 *
 *  becomes:
 *
 *  {
 *    var r = <expr>
 *    // code not using l
 *  }
 */
 class Varify(implicit val ctx: Context) extends Optimisation {
  import ast.tpd._

  val paramsTimesUsed = mutable.HashMap[Symbol, Int]()

  val possibleRenames = mutable.HashMap[Symbol, Set[Symbol]]()

  val visitor: Tree => Unit = {
    case t: ValDef
      if t.symbol.is(Param) =>
      paramsTimesUsed += (t.symbol -> 0)
    case valDef: ValDef
      if valDef.symbol.is(Mutable) =>
      valDef.rhs.foreachSubTree { subtree =>
        if (paramsTimesUsed.contains(subtree.symbol) &&
          valDef.symbol.info.widenDealias <:< subtree.symbol.info.widenDealias) {
          val newSet = possibleRenames.getOrElse(valDef.symbol, Set.empty) + subtree.symbol
          possibleRenames.put(valDef.symbol, newSet)
        }
      }
    case t: RefTree
      if paramsTimesUsed.contains(t.symbol) =>
      val param = t.symbol
      val current = paramsTimesUsed.get(param)
      current foreach { c => paramsTimesUsed += (param -> (c + 1)) }
    case _ =>
  }

  def transformer(localCtx: Context): Tree => Tree = {
    val paramCandidates = paramsTimesUsed.filter(kv => kv._2 == 1).keySet
    val renames: Map[Symbol, Symbol] = possibleRenames.iterator
      .map(kv => (kv._1, kv._2.intersect(paramCandidates)))
      .filter(x => x._2.nonEmpty)
      .map(x => (x._1, x._2.head))
      .toMap
    val transformation: Tree => Tree = {
      case t: RefTree
        if renames.contains(t.symbol) =>
        ref(renames(t.symbol))
      case t: ValDef
        if renames.contains(t.symbol) =>
        val replaced = renames(t.symbol)
        if (t.rhs.symbol == replaced) EmptyTree
        else ref(replaced).becomes(t.rhs)
      case t: ValDef
        if paramCandidates.contains(t.symbol) =>
        t.symbol.flags = Mutable
        t
      case t => t
    }
    transformation
  }
}
