package dotty.tools.dotc
package transform

import core._
import Names._
import dotty.tools.dotc.transform.MegaPhase._
import ast.Trees._
import ast.untpd
import Flags._
import Types._
import Constants.Constant
import Contexts.Context
import Symbols._
import Decorators._
import scala.collection.mutable
import DenotTransformers._
import NameOps._
import NameKinds.OuterSelectName
import StdNames._

object FirstTransform {
  val name: String = "firstTransform"
}

/** The first tree transform
 *   - eliminates some kinds of trees: Imports, NamedArgs
 *   - stubs out native methods
 *   - eliminates self tree in Template and self symbol in ClassInfo
 *   - collapses all type trees to trees of class TypeTree
 *   - converts idempotent expressions with constant types
 *   - drops branches of ifs using the rules
 *          if (true) A else B    ==> A
 *          if (false) A else B   ==> B
 */
class FirstTransform extends MiniPhase with InfoTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = FirstTransform.name

  /** eliminate self symbol in ClassInfo */
  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    case tp @ ClassInfo(_, _, _, _, self: Symbol) =>
      tp.derivedClassInfo(selfInfo = self.info)
    case _ =>
      tp
  }

  override protected def mayChange(sym: Symbol)(implicit ctx: Context): Boolean = sym.isClass

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case Select(qual, name) if !name.is(OuterSelectName) && tree.symbol.exists =>
        assert(
          qual.tpe.derivesFrom(tree.symbol.owner) ||
            tree.symbol.is(JavaStatic) && qual.tpe.derivesFrom(tree.symbol.enclosingClass),
          i"non member selection of ${tree.symbol.showLocated} from ${qual.tpe} in $tree")
      case _: TypeTree =>
      case _: Import | _: NamedArg | _: TypTree =>
        assert(false, i"illegal tree: $tree")
      case _ =>
    }
  }

  /** Reorder statements so that module classes always come after their companion classes */
  private def reorderAndComplete(stats: List[Tree])(implicit ctx: Context): List[Tree] = {
    val moduleClassDefs, singleClassDefs = mutable.Map[Name, Tree]()

    /* Returns the result of reordering stats and prepending revPrefix in reverse order to it.
     * The result of reorder is equivalent to reorder(stats, revPrefix) = revPrefix.reverse ::: reorder(stats, Nil).
     * This implementation is tail recursive as long as the element is not a module TypeDef.
     */
    def reorder(stats: List[Tree], revPrefix: List[Tree]): List[Tree] = stats match {
      case (stat: TypeDef) :: stats1 if stat.symbol.isClass =>
        if (stat.symbol.is(Flags.Module)) {
          def pushOnTop(xs: List[Tree], ys: List[Tree]): List[Tree] =
            (ys /: xs)((ys, x) => x :: ys)
          moduleClassDefs += (stat.name -> stat)
          singleClassDefs -= stat.name.stripModuleClassSuffix
          val stats1r = reorder(stats1, Nil)
          pushOnTop(revPrefix, if (moduleClassDefs contains stat.name) stat :: stats1r else stats1r)
        } else {
          reorder(
            stats1,
            moduleClassDefs remove stat.name.moduleClassName match {
              case Some(mcdef) =>
                mcdef :: stat :: revPrefix
              case None =>
                singleClassDefs += (stat.name -> stat)
                stat :: revPrefix
            }
          )
        }
      case stat :: stats1 => reorder(stats1, stat :: revPrefix)
      case Nil => revPrefix.reverse
    }

    reorder(stats, Nil)
  }

  /** eliminate self in Template */
  override def transformTemplate(impl: Template)(implicit ctx: Context): Tree = {
    cpy.Template(impl)(self = EmptyValDef)
  }

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context): Tree = {
    val meth = ddef.symbol.asTerm
    if (meth.hasAnnotation(defn.NativeAnnot)) {
      meth.resetFlag(Deferred)
      polyDefDef(meth,
        _ => _ => ref(defn.Sys_errorR).withSpan(ddef.span)
          .appliedTo(Literal(Constant(s"native method stub"))))

    }
    else ddef
  }

  override def transformStats(trees: List[Tree])(implicit ctx: Context): List[Tree] =
    ast.Trees.flatten(reorderAndComplete(trees)(ctx.withPhase(thisPhase.next)))

  private object collectBinders extends TreeAccumulator[List[Ident]] {
    def apply(annots: List[Ident], t: Tree)(implicit ctx: Context): List[Ident] = t match {
      case t @ Bind(_, body) =>
        val annot = untpd.Ident(tpnme.BOUNDTYPE_ANNOT).withType(t.symbol.typeRef)
        apply(annot :: annots, body)
      case _ =>
        foldOver(annots, t)
    }
  }

  /** Replace type tree `t` of type `T` with `TypeTree(T)`, but record all
   *  nested Bind nodes in annotations. These are interpreted in TreeTypeMaps
   *  so that bound symbols can be properly copied.
   */
  private def toTypeTree(tree: Tree)(implicit ctx: Context) = {
    val binders = collectBinders.apply(Nil, tree)
    val result: Tree = TypeTree(tree.tpe).withSpan(tree.span)
    (result /: binders)(Annotated(_, _))
  }

  override def transformOther(tree: Tree)(implicit ctx: Context): Tree = tree match {
    case tree: Import => EmptyTree
    case tree: NamedArg => transformAllDeep(tree.arg)
    case tree => if (tree.isType) toTypeTree(tree) else tree
  }

  override def transformIdent(tree: Ident)(implicit ctx: Context): Tree =
    if (tree.isType) toTypeTree(tree) else constToLiteral(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context): Tree =
    if (tree.isType) toTypeTree(tree) else constToLiteral(tree)

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree =
    constToLiteral(tree)

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree =
    constToLiteral(foldCondition(tree))

  override def transformTyped(tree: Typed)(implicit ctx: Context): Tree =
    constToLiteral(tree)

  override def transformBlock(tree: Block)(implicit ctx: Context): Tree =
    constToLiteral(tree)

  override def transformIf(tree: If)(implicit ctx: Context): Tree =
    tree.cond match {
      case Literal(Constant(c: Boolean)) => if (c) tree.thenp else tree.elsep
      case _ => tree
    }

  /** Perform one of the following simplification if applicable:
   *
   *      true  && y   ==>  y
   *      false && y   ==>  false
   *      true  || y   ==>  true
   *      false || y   ==>  y
   */
  private def foldCondition(tree: Apply)(implicit ctx: Context) = tree.fun match {
    case Select(x @ Literal(Constant(c: Boolean)), op) =>
      tree.args match {
        case y :: Nil if y.tpe.widen.isRef(defn.BooleanClass) =>
          op match {
            case nme.ZAND => if (c) y else x
            case nme.ZOR  => if (c) x else y
            case _ => tree
          }
        case _ => tree
      }
    case _ => tree
  }

  // invariants: all modules have companion objects
  // all types are TypeTrees
  // all this types are explicit
}
