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
 *    // code that may use l
 *    var r = l
 *    // code not using l
 *  }
 *
 *  becomes:
 *
 *  {
 *    var r = <expr>
 *    // code that may use l
 *    // code not using l
 *  }
 */
 class Varify extends Optimisation {
  import ast.tpd._

  val paramsTimesUsed = mutable.HashMap[Symbol, Int]()

  val possibleRenames = mutable.HashMap[Symbol, Set[Symbol]]()

  def clear(): Unit = {
    paramsTimesUsed.clear()
    possibleRenames.clear()
  }

  def visitor(implicit ctx: Context): Tree => Unit = {
    case t: ValDef if t.symbol.is(Param) =>
      paramsTimesUsed += (t.symbol -> 0)

    case t: ValDef if t.symbol.is(Mutable) =>
      t.rhs.foreachSubTree { subtree =>
        if (paramsTimesUsed.contains(subtree.symbol) &&
          t.symbol.info.widenDealias <:< subtree.symbol.info.widenDealias) {
          val newSet = possibleRenames.getOrElse(t.symbol, Set.empty) + subtree.symbol
          possibleRenames.put(t.symbol, newSet)
        }
      }

    case t: RefTree if paramsTimesUsed.contains(t.symbol) =>
      val param = t.symbol
      val current = paramsTimesUsed.get(param)
      current foreach { c => paramsTimesUsed += (param -> (c + 1)) }

    case _ =>
  }

  def transformer(implicit ctx: Context): Tree => Tree = {
    val paramCandidates = paramsTimesUsed.filter(kv => kv._2 == 1).keySet
    val renames: Map[Symbol, Symbol] = possibleRenames.iterator
      .map(kv => (kv._1, kv._2.intersect(paramCandidates)))
      .filter(x => x._2.nonEmpty)
      .map(x => (x._1, x._2.head))
      .toMap

    val transformation: Tree => Tree = {
      case t: RefTree if renames.contains(t.symbol) =>
        ref(renames(t.symbol))

      case t: ValDef if renames.contains(t.symbol) =>
        val replaced = renames(t.symbol)
        if (t.rhs.symbol == replaced) EmptyTree
        else ref(replaced).becomes(t.rhs)

      case t: ValDef if paramCandidates.contains(t.symbol) =>
        t.symbol.flags = Mutable
        t

      case t => t
    }
    transformation
  }
}
