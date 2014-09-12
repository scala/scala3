package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.SymDenotations._
import core.StdNames.nme
import core.Names._
import core.NameOps._
import ast.Trees._
import SymUtils._
import collection.{ mutable, immutable }
import collection.mutable.{ LinkedHashMap, LinkedHashSet, TreeSet }

class CapturedVars extends MiniPhaseTransform with SymTransformer { thisTransformer =>
  import ast.tpd._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "capturedVars"

  override def treeTransformPhase = thisTransformer.next

  private var captured: mutable.HashSet[Symbol] = _

  private class CollectCaptured(implicit ctx: Context) extends TreeAccumulator[Symbol] {
    def apply(enclMeth: Symbol, tree: Tree) = {
      tree match {
        case id: Ident =>
          val sym = id.symbol
          if (sym.is(Mutable, butNot = Method) && sym.owner.isTerm && sym.enclosingMethod != enclMeth) {
            ctx.log(i"capturing $sym in ${sym.enclosingMethod}, referenced from $enclMeth")
            captured += sym
          }
        case tree: DefTree if tree.symbol.exists =>
          foldOver(tree.symbol.enclosingMethod, tree)
        case _ =>
          foldOver(enclMeth, tree)
      }
      enclMeth
    }
    def runOver(tree: Tree) = {
      captured = mutable.HashSet()
      apply(NoSymbol, tree)
    }
  }

  override def init(implicit ctx: Context, info: TransformerInfo): Unit =
    (new CollectCaptured).runOver(ctx.compilationUnit.tpdTree)

  override def transformSym(sd: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (captured(sd.symbol)) {
      val newd = sd.copySymDenotation(
        info = refCls(sd.info.classSymbol, sd.hasAnnotation(defn.VolatileAnnot)).typeRef)
      newd.removeAnnotation(defn.VolatileAnnot)
      newd
    } else sd

  /** The {Volatile|}{Int|Double|...|Object}Ref class corresponding to the class `cls`,
   *  depending on whether the reference should be @volatile
   */
  def refCls(cls: Symbol, isVolatile: Boolean)(implicit ctx: Context): Symbol = {
    val refMap = if (isVolatile) defn.volatileRefClass else defn.refClass
    refMap.getOrElse(cls, refMap(defn.ObjectClass))
  }

  def capturedType(vble: Symbol)(implicit ctx: Context): Type = {
    val oldInfo = vble.denot(ctx.withPhase(thisTransformer)).info
    refCls(oldInfo.classSymbol, vble.isVolatile).typeRef
  }

  override def transformValDef(vdef: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val vble = vdef.symbol
    if (captured contains vble) {
      def boxMethod(name: TermName): Tree =
        ref(vble.info.classSymbol.companionModule.info.member(name).symbol)
      cpy.ValDef(vdef)(
        rhs = vdef.rhs match {
          case EmptyTree => boxMethod(nme.zero).appliedToNone.withPos(vdef.pos)
          case arg       => boxMethod(nme.create).appliedTo(arg)
        },
        tpt = TypeTree(vble.info).withPos(vdef.tpt.pos))
    }
    else vdef
  }

  override def transformIdent(id: Ident)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val vble = id.symbol
    if (captured(vble))
      (id select nme.elem).ensureConforms(vble.denot(ctx.withPhase(thisTransformer)).info)
    else id
  }

  override def transformAssign(tree: Assign)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val lhs1 = tree.lhs match {
      case TypeApply(Select(qual @ Select(qual2, nme.elem), nme.asInstanceOf_), _) =>
        assert(captured(qual2.symbol))
        qual
      case _ => tree.lhs
    }
    cpy.Assign(tree)(lhs1, tree.rhs)
  }
}