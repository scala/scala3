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
import core.NameKinds.TempResultName
import ast.Trees._
import SymUtils._
import collection.{ mutable, immutable }
import collection.mutable.{ LinkedHashMap, LinkedHashSet, TreeSet }

/** This phase translates variables that are captured in closures to
 *  heap-allocated refs.
 */
class CapturedVars extends MiniPhase with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "capturedVars"
  val treeTransform = new Transform(Set())

  private class RefInfo(implicit ctx: Context) {
    /** The classes for which a Ref type exists. */
    val refClassKeys: collection.Set[Symbol] =
      defn.ScalaNumericValueClasses() + defn.BooleanClass + defn.ObjectClass

    val refClass: Map[Symbol, Symbol] =
      refClassKeys.map(rc => rc -> ctx.requiredClass(s"scala.runtime.${rc.name}Ref")).toMap

    val volatileRefClass: Map[Symbol, Symbol] =
      refClassKeys.map(rc => rc -> ctx.requiredClass(s"scala.runtime.Volatile${rc.name}Ref")).toMap

    val boxedRefClasses: collection.Set[Symbol] =
      refClassKeys.flatMap(k => Set(refClass(k), volatileRefClass(k)))
  }

  class Transform(captured: collection.Set[Symbol]) extends TreeTransform {
    def phase = thisTransform

    private var myRefInfo: RefInfo = null
    private def refInfo(implicit ctx: Context) = {
      if (myRefInfo == null) myRefInfo = new RefInfo()
      myRefInfo
    }

    private class CollectCaptured extends TreeTraverser {
      private val captured = mutable.HashSet[Symbol]()
      def traverse(tree: Tree)(implicit ctx: Context) = tree match {
        case id: Ident =>
          val sym = id.symbol
          if (sym.is(Mutable, butNot = Method) && sym.owner.isTerm) {
            val enclMeth = ctx.owner.enclosingMethod
            if (sym.enclosingMethod != enclMeth) {
              ctx.log(i"capturing $sym in ${sym.enclosingMethod}, referenced from $enclMeth")
              captured += sym
            }
          }
        case _ =>
          traverseChildren(tree)
      }
      def runOver(tree: Tree)(implicit ctx: Context): collection.Set[Symbol] = {
        traverse(tree)
        captured
      }
    }

    override def prepareForUnit(tree: Tree)(implicit ctx: Context) = {
      val captured = (new CollectCaptured)
        .runOver(ctx.compilationUnit.tpdTree)(ctx.withPhase(thisTransform))
      new Transform(captured)
    }

    /** The {Volatile|}{Int|Double|...|Object}Ref class corresponding to the class `cls`,
     *  depending on whether the reference should be @volatile
     */
    def refClass(cls: Symbol, isVolatile: Boolean)(implicit ctx: Context): Symbol = {
      val refMap = if (isVolatile) refInfo.volatileRefClass else refInfo.refClass
      if (cls.isClass)  {
        refMap.getOrElse(cls, refMap(defn.ObjectClass))
      }
      else refMap(defn.ObjectClass)
    }

    override def prepareForValDef(vdef: ValDef)(implicit ctx: Context) = {
      val sym = vdef.symbol
      if (captured contains sym) {
        val newd = sym.denot(ctx.withPhase(thisTransform)).copySymDenotation(
          info = refClass(sym.info.classSymbol, sym.hasAnnotation(defn.VolatileAnnot)).typeRef,
          initFlags = sym.flags &~ Mutable)
        newd.removeAnnotation(defn.VolatileAnnot)
        newd.installAfter(thisTransform)
      }
      this
    }

    override def transformValDef(vdef: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
      val vble = vdef.symbol
      if (captured contains vble) {
        def boxMethod(name: TermName): Tree =
          ref(vble.info.classSymbol.companionModule.info.member(name).symbol)
        cpy.ValDef(vdef)(
          rhs = vdef.rhs match {
            case EmptyTree => boxMethod(nme.zero).appliedToNone.withPos(vdef.pos)
            case arg => boxMethod(nme.create).appliedTo(arg)
          },
          tpt = TypeTree(vble.info).withPos(vdef.tpt.pos))
      } else vdef
    }

    override def transformIdent(id: Ident)(implicit ctx: Context, info: TransformerInfo): Tree = {
      val vble = id.symbol
      if (captured(vble))
        (id select nme.elem).ensureConforms(vble.denot(ctx.withPhase(thisTransform)).info)
      else id
    }

    /** If assignment is to a boxed ref type, e.g.
     *
     *      intRef.elem = expr
     *
     *  rewrite using a temporary var to
     *
     *      val ev$n = expr
     *      intRef.elem = ev$n
     *
     *  That way, we avoid the problem that `expr` might contain a `try` that would
     *  run on a non-empty stack (which is illegal under JVM rules). Note that LiftTry
     *  has already run before, so such `try`s would not be eliminated.
     *
     *  Also: If the ref type lhs is followed by a cast (can be an artifact of nested translation),
     *  drop the cast.
     */
    override def transformAssign(tree: Assign)(implicit ctx: Context, info: TransformerInfo): Tree = {
      def recur(lhs: Tree): Tree = lhs match {
        case TypeApply(Select(qual, nme.asInstanceOf_), _) =>
          val Select(_, nme.elem) = qual
          recur(qual)
        case Select(_, nme.elem) if refInfo.boxedRefClasses.contains(lhs.symbol.maybeOwner) =>
          val tempDef = transformFollowing(SyntheticValDef(TempResultName.fresh(), tree.rhs))
          transformFollowing(Block(tempDef :: Nil, cpy.Assign(tree)(lhs, ref(tempDef.symbol))))
        case _ =>
          tree
      }
      recur(tree.lhs)
    }
  }
}
