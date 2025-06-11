package dotty.tools.dotc.qualified_types

import scala.util.hashing.MurmurHash3 as hashing

import dotty.tools.dotc.ast.tpd.{closureDef, Apply, Block, DefDef, Ident, Literal, New, Select, Tree, TreeOps, TypeApply, Typed, TypeTree}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{MethodType, TermRef, Type, TypeVar}
import dotty.tools.dotc.core.Symbols.defn

import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.config.Printers

private abstract class QualifierComparer:
  private def typeIso(tp1: Type, tp2: Type) =
    val tp1stripped = stripPermanentTypeVar(tp1)
    val tp2stripped = stripPermanentTypeVar(tp2)
    tp1stripped.equals(tp2stripped)

  /** Structural equality for trees.
   *
   *  This implementation is _not_ alpha-equivalence aware, which
   *    1. allows it not to rely on a [[Context]] and,
   *    2. allows the corresponding [[hash]] method to reuse [[Type#hashCode]]
   *       instead of defining an other hash code for types.
   */
  def iso(tree1: Tree, tree2: Tree): Boolean =
    (tree1, tree2) match
      case (Literal(_) | Ident(_), _) =>
        typeIso(tree1.tpe, tree2.tpe)
      case (Select(qual1, name1), Select(qual2, name2)) =>
        name1 == name2 && iso(qual1, qual2)
      case (Apply(fun1, args1), Apply(fun2, args2)) =>
        iso(fun1, fun2) && args1.corresponds(args2)(iso)
      case (TypeApply(fun1, args1), TypeApply(fun2, args2)) =>
        iso(fun1, fun2) && args1.corresponds(args2)((arg1, arg2) => typeIso(arg1.tpe, arg2.tpe))
      case (tpt1: TypeTree, tpt2: TypeTree) =>
        typeIso(tpt1.tpe, tpt2.tpe)
      case (Typed(expr1, tpt1), Typed(expr2, tpt2)) =>
        iso(expr1, expr2) && typeIso(tpt1.tpe, tpt2.tpe)
      case (New(tpt1), New(tpt2)) =>
        typeIso(tpt1.tpe, tpt2.tpe)
      case (Block(stats1, expr1), Block(stats2, expr2)) =>
        stats1.corresponds(stats2)(iso) && iso(expr1, expr2)
      case _ =>
        tree1.equals(tree2)

  protected def stripPermanentTypeVar(tp: Type): Type =
    tp match
      case tp: TypeVar if tp.isPermanentlyInstantiated => tp.permanentInst
      case tp                                          => tp

private[qualified_types] object QualifierStructuralComparer extends QualifierComparer:
  /** A hash code for trees that corresponds to `iso(tree1, tree2)`. */
  def hash(tree: Tree): Int =
    tree match
      case Literal(_) | Ident(_) =>
        hashType(tree.tpe)
      case Select(qual, name) =>
        hashing.mix(name.hashCode, hash(qual))
      case Apply(fun, args) =>
        hashing.mix(hash(fun), hashList(args))
      case TypeApply(fun, args) =>
        hashing.mix(hash(fun), hashList(args))
      case tpt: TypeTree =>
        hashType(tpt.tpe)
      case Typed(expr, tpt) =>
        hashing.mix(hash(expr), hashType(tpt.tpe))
      case New(tpt1) =>
        hashType(tpt1.tpe)
      case Block(stats, expr) =>
        hashing.mix(hashList(stats), hash(expr))
      case _ =>
        tree.hashCode

  private def hashList(trees: List[Tree]): Int =
    trees.map(hash).foldLeft(0)(hashing.mix)

  private def hashType(tp: Type): Int =
    stripPermanentTypeVar(tp).hashCode

  /** A box for trees that implements structural equality using [[iso]] and
   *  [[hash]]. This enables using trees as keys in hash maps.
   */
  final class TreeBox(val tree: Tree) extends AnyVal:
    override def equals(that: Any): Boolean = that match
      case that: TreeBox => iso(tree, that.tree)
      case _             => false

    override def hashCode: Int = hash(tree)

private[qualified_types] final class QualifierAlphaComparer(using Context) extends QualifierComparer:
  override def iso(tree1: Tree, tree2: Tree): Boolean =
    trace(i"iso $tree1 ; $tree2"):
      (tree1, tree2) match
        case (closureDef(def1), closureDef(def2)) =>
          val def2substituted = def2.rhs.subst(def2.symbol.paramSymss.flatten, def1.symbol.paramSymss.flatten)
          val def2normalized = QualifierNormalizer.normalize(def2substituted)
          iso(def1.rhs, def2normalized)
        case _ =>
          super.iso(tree1, tree2)
