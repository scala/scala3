package dotty.tools.dotc
package transform

import MegaPhase._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Flags._
import core.Decorators._
import core.StdNames.nme
import core.Names._
import core.NameKinds.TempResultName
import core.Constants._
import util.Store
import dotty.tools.uncheckedNN
import ast.tpd.*
import compiletime.uninitialized

/** This phase translates variables that are captured in closures to
 *  heap-allocated refs.
 */
class CapturedVars extends MiniPhase with IdentityDenotTransformer:
  thisPhase =>

  override def phaseName: String = CapturedVars.name

  override def description: String = CapturedVars.description

  private val captured = util.HashSet[Symbol]()

  private class RefInfo(using Context) {
    /** The classes for which a Ref type exists. */
    val refClassKeys: collection.Set[Symbol] =
      defn.ScalaNumericValueClasses() `union` Set(defn.BooleanClass, defn.ObjectClass)

    val refClass: Map[Symbol, Symbol] =
      refClassKeys.map(rc => rc -> requiredClass(s"scala.runtime.${rc.name}Ref")).toMap

    val volatileRefClass: Map[Symbol, Symbol] =
      refClassKeys.map(rc => rc -> requiredClass(s"scala.runtime.Volatile${rc.name}Ref")).toMap

    val boxedRefClasses: collection.Set[Symbol] =
      refClassKeys.flatMap(k => Set(refClass(k), volatileRefClass(k)))

    val objectRefClasses: collection.Set[Symbol] =
      Set(refClass(defn.ObjectClass), volatileRefClass(defn.ObjectClass))
  }

  private var myRefInfo: RefInfo | Null = null
  private def refInfo(using Context): RefInfo = {
    if (myRefInfo == null) myRefInfo = new RefInfo()
    myRefInfo.uncheckedNN
  }

  override def prepareForUnit(tree: Tree)(using Context): Context =
    captured.clear()
    atPhase(thisPhase)(CapturedVars.collect(captured)).traverse(tree)
    ctx

  /** The {Volatile|}{Int|Double|...|Object}Ref class corresponding to the class `cls`,
    *  depending on whether the reference should be @volatile
    */
  def refClass(cls: Symbol, isVolatile: Boolean)(using Context): Symbol = {
    val refMap = if (isVolatile) refInfo.volatileRefClass else refInfo.refClass
    if (cls.isClass)
      refMap.getOrElse(cls, refMap(defn.ObjectClass))
    else refMap(defn.ObjectClass)
  }

  override def prepareForValDef(vdef: ValDef)(using Context): Context = {
    val sym = atPhase(thisPhase)(vdef.symbol)
    if (captured contains sym) {
      val newd = atPhase(thisPhase)(sym.denot).copySymDenotation(
        info = refClass(sym.info.classSymbol, sym.hasAnnotation(defn.VolatileAnnot)).typeRef,
        initFlags = sym.flags &~ Mutable)
      newd.removeAnnotation(defn.VolatileAnnot)
      newd.installAfter(thisPhase)
    }
    ctx
  }

  override def transformValDef(vdef: ValDef)(using Context): Tree = {
    val vble = vdef.symbol
    if (captured.contains(vble)) {
      def boxMethod(name: TermName): Tree =
        ref(vble.info.classSymbol.companionModule.info.member(name).symbol)
      cpy.ValDef(vdef)(
        rhs = boxMethod(nme.create).appliedTo(vdef.rhs),
        tpt = TypeTree(vble.info).withSpan(vdef.tpt.span))
    }
    else vdef
  }

  override def transformIdent(id: Ident)(using Context): Tree = {
    val vble = id.symbol
    if (captured.contains(vble))
      id.select(nme.elem).ensureConforms(atPhase(thisPhase)(vble.denot).info)
    else id
  }

  /** If assignment is to a boxed ref type, e.g.
   *
   *      intRef.elem = expr
   *
   *  the lhs can be followed by a cast as an artifact of nested translation.
   *  In that case, drop the cast.
   */
  override def transformAssign(tree: Assign)(using Context): Tree =
    tree.lhs match
      case TypeApply(Select(qual@Select(_, nme.elem), nme.asInstanceOf_), _) =>
        cpy.Assign(tree)(qual, tree.rhs)
      case _ =>
        tree

object CapturedVars:
  val name: String = "capturedVars"
  val description: String = "represent vars captured by closures as heap objects"

  def collect(captured: util.HashSet[Symbol]): TreeTraverser = new:
    def traverse(tree: Tree)(using Context) = tree match
      case id: Ident =>
        val sym = id.symbol
        if sym.is(Mutable, butNot = Method) && sym.owner.isTerm then
          val enclMeth = ctx.owner.enclosingMethod
          if sym.enclosingMethod != enclMeth then
            report.log(i"capturing $sym in ${sym.enclosingMethod}, referenced from $enclMeth")
            captured += sym
      case _ =>
        traverseChildren(tree)
end CapturedVars
