package dotty.tools.dotc
package transform.localopt

import core.Contexts.Context
import core.Symbols._
import ast.Trees._
import dotty.tools.dotc.ast.tpd

/** If a block has a statement that evaluates to Nothing:
 *   - Every pure statement dirrectly preceding an expression that returns Nothing can be removed,
 *   - as every statement after an expression that returns Nothing can be removed
 *
 *  If an If condition evalutates to Nothing, the entire If can be replaced by condition
 *  If an argument evaluates to Nothing, the entire call can be replaced by evaluation of arguments.
 *
 *  This optimisation makes it rather tricky to write meaningful examples
 *  since the compiler will often be able to reduce them to a single main
 *  method with body = ???.
 *
 *  @author DarkDimius, OlivierBlanvillain
 */
class BubbleUpNothing extends Optimisation {
  import ast.tpd._

  def visitor(implicit ctx: Context) = NoVisitor
  def clear(): Unit = ()

  def transformer(implicit ctx: Context): Tree => Tree = {
    case t @ Apply(Select(Notathing(qual), _), args) =>
      Typed(qual, TypeTree(t.tpe))
    // This case leads to complications with multiple argument lists,
    // how to do you rewrites tree.witType(???)(ctx).withType(???)(ctx)
    // using Ycheckable steps?

    // Solution: only transform when having a complete application,
    // steal code from tailRec

    // case t @ Apply(Select(qual, _), args) if args.exists(notathing) =>
    //   val (keep, noth :: other) = args.span(x => !notathing(x))
    //   Block(qual :: keep, Typed(noth, TypeTree(t.tpe)))
    case Assign(_, rhs) if notathing(rhs) =>
      rhs
    case t @ If(Notathing(cond), _, _) =>
      Typed(cond, TypeTree(t.tpe))
    case b: Block if b.stats.exists(x => !x.isDef && notathing(x)) =>
      val (keep, noth :: other) = b.stats.span(x => x.isDef || !notathing(x))
      val keepDefs = other.filter(x => x.isDef)
      val body = keep ::: keepDefs
      Typed(Block(body, noth), TypeTree(b.tpe))
    case t => t
  }

  object Notathing {
    def unapply(t: Tree)(implicit ctx: Context): Option[Tree] = Option(lookup(t))
    def lookup(t: Tree)(implicit ctx: Context): Tree = t match {
      case x if x.tpe.derivesFrom(defn.NothingClass) => t
      case Typed(x, _) => lookup(x)
      case Block(_, x) => lookup(x)
      case _ => null
    }
  }

  def notathing(t: Tree)(implicit ctx: Context): Boolean = t match {
    case Notathing(_) => true
    case _ => false
  }
}
