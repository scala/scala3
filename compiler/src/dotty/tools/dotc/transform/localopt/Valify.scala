package dotty.tools.dotc
package transform.localopt

import core.Contexts.Context
import core.Symbols._
import core.Types._
import core.Flags._
import ast.Trees._
import scala.collection.mutable

/** Rewrite vars with exactly one assignment as vals.
 *
 *  @author DarkDimius, OlivierBlanvillain
 */
class Valify(val simplifyPhase: Simplify) extends Optimisation {
  import ast.tpd._

  // Either a duplicate or a read through series of immutable fields.
  val defined: mutable.Map[Symbol, ValDef] = mutable.Map()

  val firstRead: mutable.Map[Symbol, RefTree] = mutable.Map()

  val firstWrite: mutable.Map[Symbol, Assign] = mutable.Map()

  val secondWrite: mutable.Map[Symbol, Assign] = mutable.Map()

  def clear(): Unit = {
    defined.clear()
    firstRead.clear()
    firstWrite.clear()
    secondWrite.clear()
  }

  def visitor(implicit ctx: Context): Tree => Unit = {
    case t: ValDef if t.symbol.is(Mutable, Lazy) && !t.symbol.is(Method) && !t.symbol.owner.isClass =>
      if (isPureExpr(t.rhs))
        defined(t.symbol) = t

    case t: RefTree if t.symbol.exists && !t.symbol.is(Method) && !t.symbol.owner.isClass =>
      if (!firstWrite.contains(t.symbol)) firstRead(t.symbol) = t

    case t @ Assign(l, expr) if !l.symbol.is(Method) && !l.symbol.owner.isClass =>
      if (!firstRead.contains(l.symbol)) {
        if (firstWrite.contains(l.symbol)) {
          if (!secondWrite.contains(l.symbol))
            secondWrite(l.symbol) = t
        } else if (!expr.existsSubTree(x => x match {
          case tree: RefTree if x.symbol == l.symbol => firstRead(l.symbol) = tree; true
          case _ => false
        })) {
          firstWrite(l.symbol) = t
        }
      }
    case _ =>
  }

  def transformer(implicit ctx: Context): Tree => Tree = {
    case t: Block => // Drop non-side-effecting stats
      val valdefs = t.stats.collect {
        case t: ValDef if defined.contains(t.symbol) => t
      }

      val assigns = t.stats.filter {
        case t @ Assign(lhs, r) =>
          firstWrite.contains(lhs.symbol) && !secondWrite.contains(lhs.symbol)
        case _ => false
      }

      val pairs = valdefs.flatMap(x => assigns.find(y => y.asInstanceOf[Assign].lhs.symbol == x.symbol) match {
        case Some(y: Assign) => List((x, y))
        case _ => Nil
      })

      val valsToDrop = pairs.map(_._1).toSet
      val assignsToReplace: Map[Assign, ValDef] = pairs.map(_.swap).toMap

      val newStats = t.stats.mapConserve {
        case x: ValDef if valsToDrop.contains(x) => EmptyTree
        case t: Assign => assignsToReplace.get(t) match {
          case Some(vd) =>
            val newD = vd.symbol.asSymDenotation.copySymDenotation(initFlags = vd.symbol.flags.&~(Mutable))
            newD.installAfter(simplifyPhase)
            ValDef(vd.symbol.asTerm, t.rhs)
          case None => t
        }
        case x => x
      }

      if (newStats eq t.stats) t
      else cpy.Block(t)(newStats, t.expr)
    case tree => tree
  }
}
