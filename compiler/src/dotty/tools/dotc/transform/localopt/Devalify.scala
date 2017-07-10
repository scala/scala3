package dotty.tools.dotc
package transform.localopt

import core.Constants.Constant
import core.Contexts.Context
import core.Flags._
import core.Symbols._
import core.Types._
import ast.Trees._
import scala.collection.mutable
import config.Printers.simplify
import Simplify._
import transform.SymUtils._

/** Inline vals and remove vals that are aliases to other vals
 *
 *  Notion of alias is a by-value notion, so "good" casts are ignored.
 *
 *  This phase has to be careful not to eliminate vals that are parts of other types
 *
 *  @author DarkDimius, OlivierBlanvillain
 */
class Devalify extends Optimisation {
  import ast.tpd._

  val timesUsed = mutable.HashMap[Symbol, Int]()
  val timesUsedAsType = mutable.HashMap[Symbol, Int]()

  val defined = mutable.HashSet[Symbol]()
  val usedInInnerClass = mutable.HashMap[Symbol, Int]()
  // Either a duplicate or a read through series of immutable fields
  val copies = mutable.HashMap[Symbol, Tree]()

  def clear(): Unit = {
    timesUsed.clear()
    timesUsedAsType.clear()
    defined.clear()
    usedInInnerClass.clear()
    copies.clear()
  }

  def visitType(tp: Type)(implicit ctx: Context): Unit = {
    tp.foreachPart(x => x match {
      case TermRef(NoPrefix, _) =>
        val b4 = timesUsedAsType.getOrElseUpdate(x.termSymbol, 0)
        timesUsedAsType.put(x.termSymbol, b4 + 1)
      case _ =>
    })
  }

  def doVisit(tree: Tree, used: mutable.HashMap[Symbol, Int])(implicit ctx: Context): Unit = tree match {
    case valdef: ValDef if !valdef.symbol.is(Param | Mutable | Module | Lazy) &&
                           valdef.symbol.exists && !valdef.symbol.owner.isClass =>
      defined += valdef.symbol

      dropCasts(valdef.rhs) match {
        case t: Tree if readingOnlyVals(t) =>
          copies.put(valdef.symbol, valdef.rhs)
        case _ =>
      }
      visitType(valdef.symbol.info)
    case t: New =>
      val normalized = t.tpt.tpe.normalizedPrefix
      val symIfExists = normalized.termSymbol
      val b4 = used.getOrElseUpdate(symIfExists, 0)
      used.put(symIfExists, b4 + 1)
      visitType(normalized)

    case valdef: ValDef if valdef.symbol.exists && !valdef.symbol.owner.isClass &&
                           !valdef.symbol.is(Param | Module | Lazy) =>
      // TODO: handle params after constructors. Start changing public signatures by eliminating unused arguments.
      defined += valdef.symbol

    case valdef: ValDef => visitType(valdef.symbol.info)
    case t: DefDef      => visitType(t.symbol.info)
    case t: Typed       => visitType(t.tpt.tpe)
    case t: TypeApply   => t.args.foreach(x => visitType(x.tpe))
    case t: RefTree =>
      val b4 = used.getOrElseUpdate(t.symbol, 0)
      used.put(t.symbol, b4 + 1)
    case _ =>
  }

  def visitor(implicit ctx: Context): Tree => Unit = { tree =>
    def crossingClassBoundaries(t: Tree): Boolean = t match {
      case _: New      => true
      case _: Template => true
      case _           => false
    }
    // We shouldn't inline `This` nodes, which we approximate by not inlining
    // anything across class boundaries. To do so, we visit every class a
    // second time and record what's used in the usedInInnerClass Set.
    if (crossingClassBoundaries(tree)) {
      // Doing a foreachSubTree(tree) here would work, but would also
      // be exponential for deeply nested classes. Instead we do a short
      // circuit traversal that doesn't visit further nested classes.
      val reVisitClass = new TreeAccumulator[Unit] {
        def apply(u: Unit, t: Tree)(implicit ctx: Context): Unit = {
          doVisit(t, usedInInnerClass)
          if (!crossingClassBoundaries(t))
            foldOver((), t)
        }
      }
      reVisitClass.foldOver((), tree)
    }
    doVisit(tree, timesUsed)
  }

  def transformer(implicit ctx: Context): Tree => Tree = {
    val valsToDrop = defined -- timesUsed.keySet -- timesUsedAsType.keySet
    val copiesToReplaceAsDuplicates = copies.filter { x =>
      val rhs = dropCasts(x._2)
      rhs.isInstanceOf[Literal] || (!rhs.symbol.owner.isClass && !rhs.symbol.is(Method | Mutable))
    } -- timesUsedAsType.keySet
    // TODO: if a non-synthetic val is duplicate of a synthetic one, rename a synthetic one and drop synthetic flag?

    val copiesToReplaceAsUsedOnce =
      timesUsed.filter(x => x._2 == 1)
        .flatMap(x => copies.get(x._1) match {
          case Some(tr) => List((x._1, tr))
          case None => Nil
        }) -- timesUsedAsType.keySet

    val replacements = copiesToReplaceAsDuplicates ++ copiesToReplaceAsUsedOnce -- usedInInnerClass.keySet

    val deepReplacer = new TreeMap() {
      override def transform(tree: Tree)(implicit ctx: Context): Tree = {
        def loop(tree: Tree): Tree  =
          tree match {
            case t: RefTree if replacements.contains(t.symbol) =>
              loop(replacements(t.symbol))
            case _ => tree
          }
        super.transform(loop(tree))
      }
    }

    val transformation: Tree => Tree = {
      case t: ValDef if valsToDrop.contains(t.symbol) =>
        // TODO: Could emit a warning for non synthetic code? This valdef is
        // probably something users would want to remove from source...
        simplify.println(s"Dropping definition of ${t.symbol.showFullName} as not used")
        t.rhs.changeOwner(t.symbol, t.symbol.owner)
      case t: ValDef if replacements.contains(t.symbol) =>
        simplify.println(s"Dropping definition of ${t.symbol.showFullName} as an alias")
        EmptyTree
      case t: New =>
        val symIfExists = t.tpt.tpe.normalizedPrefix.termSymbol
        if (replacements.contains(symIfExists)) {
          val newPrefix = deepReplacer.transform(replacements(symIfExists))
          val newTpt = t.tpt.tpe match {
            case t: NamedType =>
              t.derivedSelect(newPrefix.tpe)
          }
          New(newTpt)
        }
        else t
      case t: RefTree if !t.symbol.is(Method | Param | Mutable) =>
        if (replacements.contains(t.symbol))
          deepReplacer.transform(replacements(t.symbol)).ensureConforms(t.tpe.widen)
        else t
      case tree => tree
    }

    transformation
  }

  def dropCasts(t: Tree)(implicit ctx: Context): Tree = t match {
    // case TypeApply(aio@Select(rec, nm), _) if aio.symbol == defn.Any_asInstanceOf => dropCasts(rec)
    case Typed(t, tpe) => t
    case _ => t
  }

  def readingOnlyVals(t: Tree)(implicit ctx: Context): Boolean = dropCasts(t) match {
    case Typed(exp, _) => readingOnlyVals(exp)

    case TypeApply(fun @ Select(rec, _), List(tp)) =>
      val isAsInstanceOf = fun.symbol == defn.Any_asInstanceOf && rec.tpe.derivesFrom(tp.tpe.classSymbol)
      isAsInstanceOf && readingOnlyVals(rec)

    case t @ Apply(Select(rec, _), Nil) =>
      isImmutableAccessor(t) && readingOnlyVals(rec)

    case t @ Select(rec, _) if t.symbol.is(Method) =>
      isImmutableAccessor(t) && readingOnlyVals(rec)

    case t @ Select(qual, _) if !isEffectivelyMutable(t) =>
      readingOnlyVals(qual)

    case t: Ident if !t.symbol.is(Mutable | Method) && !t.symbol.info.dealias.isInstanceOf[ExprType] =>
      desugarIdent(t).forall(readingOnlyVals)

    case t: This => true
    // null => false, or the following fails devalify:
    // trait I {
    //   def foo: Any = null
    // }
    // object Main {
    //   def main = {
    //     val s: I = null
    //     s.foo
    //   }
    // }
    case Literal(Constant(null)) => false
    case t: Literal => true
    case _ => false
  }
}
