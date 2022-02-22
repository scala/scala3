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

/** This phase translates variables that are captured in closures to
 *  heap-allocated refs.
 */
class CapturedVars extends MiniPhase with IdentityDenotTransformer:
  thisPhase =>
  import ast.tpd._

  override def phaseName: String = CapturedVars.name

  override def description: String = CapturedVars.description

  override def runsAfterGroupsOf: Set[String] = Set(LiftTry.name)
    // lifting tries changes what variables are considered to be captured

  private[this] var Captured: Store.Location[util.ReadOnlySet[Symbol]] = _
  private def captured(using Context) = ctx.store(Captured)

  override def initContext(ctx: FreshContext): Unit =
    Captured = ctx.addLocation(util.ReadOnlySet.empty)

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

  private var myRefInfo: RefInfo = null
  private def refInfo(using Context) = {
    if (myRefInfo == null) myRefInfo = new RefInfo()
    myRefInfo
  }

  private class CollectCaptured extends TreeTraverser {
    private val captured = util.HashSet[Symbol]()
    def traverse(tree: Tree)(using Context) = tree match {
      case id: Ident =>
        val sym = id.symbol
        if (sym.is(Mutable, butNot = Method) && sym.owner.isTerm) {
          val enclMeth = ctx.owner.enclosingMethod
          if (sym.enclosingMethod != enclMeth) {
            report.log(i"capturing $sym in ${sym.enclosingMethod}, referenced from $enclMeth")
            captured += sym
          }
        }
      case _ =>
        traverseChildren(tree)
    }
    def runOver(tree: Tree)(using Context): util.ReadOnlySet[Symbol] = {
      traverse(tree)
      captured
    }
  }

  override def prepareForUnit(tree: Tree)(using Context): Context = {
    val captured = atPhase(thisPhase) {
      CollectCaptured().runOver(ctx.compilationUnit.tpdTree)
    }
    ctx.fresh.updateStore(Captured, captured)
  }

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
   *  rewrite using a temporary var to
   *
   *      val ev$n = expr
   *      intRef.elem = ev$n
   *
   *  That way, we avoid the problem that `expr` might contain a `try` that would
   *  run on a non-empty stack (which is illegal under JVM rules). Note that LiftTry
   *  has already run before, so such `try`s would not be eliminated.
   *
   *  If the ref type lhs is followed by a cast (can be an artifact of nested translation),
   *  drop the cast.
   *
   *  If the ref type is `ObjectRef` or `VolatileObjectRef`, immediately assign `null`
   *  to the temporary to make the underlying target of the reference available for
   *  garbage collection. Nullification is omitted if the `expr` is already `null`.
   *
   *      var ev$n: RHS = expr
   *      objRef.elem = ev$n
   *      ev$n = null.asInstanceOf[RHS]
   */
  override def transformAssign(tree: Assign)(using Context): Tree =
    def absolved: Boolean = tree.rhs match
      case Literal(Constant(null)) | Typed(Literal(Constant(null)), _) => true
      case _ => false
    def recur(lhs: Tree): Tree = lhs match
      case TypeApply(Select(qual@Select(_, nme.elem), nme.asInstanceOf_), _) =>
        recur(qual)
      case Select(_, nme.elem) if refInfo.boxedRefClasses.contains(lhs.symbol.maybeOwner) =>
        val tempDef = transformFollowing(SyntheticValDef(TempResultName.fresh(), tree.rhs, flags = Mutable))
        val update  = cpy.Assign(tree)(lhs, ref(tempDef.symbol))
        def reset   = cpy.Assign(tree)(ref(tempDef.symbol), nullLiteral.cast(tempDef.symbol.info))
        val res     = if refInfo.objectRefClasses(lhs.symbol.maybeOwner) && !absolved then reset else unitLiteral
        transformFollowing(Block(tempDef :: update :: Nil, res))
      case _ =>
        tree
    recur(tree.lhs)

object CapturedVars:
  val name: String = "capturedVars"
  val description: String = "represent vars captured by closures as heap objects"
