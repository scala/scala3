package dotty.tools
package dotc
package typer

import core._
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types._
import Symbols._
import StdNames._
import Decorators._
import ProtoTypes._
import Inferencing.isFullyDefined
import config.Printers.{capt, recheckr}
import ast.{tpd, untpd, Trees}
import NameKinds.{DocArtifactName, OuterSelectName, DefaultGetterName}
import Trees._
import scala.util.control.NonFatal
import typer.ErrorReporting._
import util.Spans.Span
import util.{SimpleIdentitySet, EqHashMap, SrcPos}
import util.Chars.*
import Nullables._
import transform.*
import transform.SymUtils.*
import scala.collection.mutable
import reporting._
import ProtoTypes._
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions
import CaptureSet.CompareResult

object CheckCaptures:
  case class Env(owner: Symbol, captured: CaptureSet, isBoxed: Boolean, outer: Env):
    def isOpen = !captured.isEmpty && !isBoxed

  extension (tp: Type)

    /** If this is  type variable instantiated or upper bounded with a capturing type,
     *  the capture set associated with that type. Extended to and-or types and
     *  type proxies in the obvious way. If a term has a type with a boxed captureset,
     *  that captureset counts towards the capture variables of the envirionment.
     */
    def boxedCaptured(using Context): CaptureSet =
      def getBoxed(tp: Type, enabled: Boolean): CaptureSet = tp match
        case tp: CapturingType if enabled => tp.refs
        case tp: TypeVar => getBoxed(tp.underlying, enabled = true)
        case tp: TypeProxy => getBoxed(tp.superType, enabled)
        case tp: AndType => getBoxed(tp.tp1, enabled) ++ getBoxed(tp.tp2, enabled)
        case tp: OrType => getBoxed(tp.tp1, enabled) ** getBoxed(tp.tp2, enabled)
        case _ => CaptureSet.empty
      getBoxed(tp, enabled = false)

    /** If this type appears as an expected type of a term, does it imply
     *  that the term should be boxed?
     */
    def needsBox(using Context): Boolean = tp match
      case _: TypeVar => true
      case tp: TypeRef =>
        tp.info match
          case TypeBounds(lo, _) => lo.needsBox
          case _ => false
      case tp: RefinedOrRecType => tp.parent.needsBox
      case tp: AnnotatedType => tp.parent.needsBox
      case tp: LazyRef => tp.ref.needsBox
      case tp: AndType => tp.tp1.needsBox || tp.tp2.needsBox
      case tp: OrType => tp.tp1.needsBox && tp.tp2.needsBox
      case _ => false
  end extension

class CheckCaptures extends Recheck:
  import ast.tpd.*
  import CheckCaptures.*

  def phaseName: String = "cc"
  override def isEnabled(using Context) = ctx.settings.Ycc.value

  def newRechecker()(using Context) = CaptureChecker(ctx)

  class CaptureChecker(ictx: Context) extends Rechecker(ictx):
    import ast.tpd.*

    override def reinfer(tp: Type)(using Context): Type =
      CapturingType(tp, CaptureSet.Var())

    private var curEnv: Env = Env(NoSymbol, CaptureSet.empty, false, null)

    private val myCapturedVars: util.EqHashMap[Symbol, CaptureSet] = EqHashMap()
    def capturedVars(sym: Symbol)(using Context) =
      myCapturedVars.getOrElseUpdate(sym,
        if sym.ownersIterator.exists(_.isTerm) then CaptureSet.Var()
        else CaptureSet.empty)

    def markFree(sym: Symbol, pos: SrcPos)(using Context): Unit =
      if sym.exists then
        val ref = sym.termRef
        def recur(env: Env): Unit =
          if env.isOpen && env.owner != sym.enclosure then
            checkElem(ref, env.captured, pos)
            recur(env.outer)
        if ref.isTracked then recur(curEnv)

    def assertSub(cs1: CaptureSet, cs2: CaptureSet)(using Context) =
      assert((cs1 <:< cs2) == CompareResult.OK, i"$cs1 is not a subset of $cs2")

    def checkElem(elem: CaptureRef, cs: CaptureSet, pos: SrcPos)(using Context) =
      val res = elem.singletonCaptureSet <:< cs
      if res != CompareResult.OK then
        report.error(i"$elem cannot be referenced here; it is not included in allowed capture set ${res.blocking}", pos)

    def checkSubset(cs1: CaptureSet, cs2: CaptureSet, pos: SrcPos)(using Context) =
      val res = cs1 <:< cs2
      if res != CompareResult.OK then
        report.error(i"references $cs1 are not all included in allowed capture set ${res.blocking}", pos)

    override def recheckClosure(tree: Closure, pt: Type)(using Context): Type =
      val cs = capturedVars(tree.meth.symbol)
      recheckr.println(i"typing closure $tree with cvs $cs")
      super.recheckClosure(tree, pt).capturing(cs)
        .showing(i"rechecked $tree, $result", capt)

    override def recheckIdent(tree: Ident)(using Context): Type =
      markFree(tree.symbol, tree.srcPos)
      super.recheckIdent(tree)

    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      val saved = curEnv
      val localSet = capturedVars(sym)
      if !localSet.isEmpty then curEnv = Env(sym, localSet, false, curEnv)
      try super.recheckDefDef(tree, sym)
      finally curEnv = saved

    override def recheckClassDef(tree: TypeDef, impl: Template, sym: ClassSymbol)(using Context): Type =
      val saved = curEnv
      val localSet = capturedVars(sym)
      if !localSet.isEmpty then curEnv = Env(sym, localSet, false, curEnv)
      try super.recheckClassDef(tree, impl, sym)
      finally curEnv = saved

    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      if curEnv.isOpen then
        val ownEnclosure = ctx.owner.enclosingMethodOrClass
        var targetSet = capturedVars(tree.symbol)
        if !targetSet.isEmpty && tree.symbol.enclosure == ownEnclosure then
          targetSet = targetSet.filter {
            case ref: TermRef => ref.symbol.enclosure != ownEnclosure
            case _ => true
          }

        checkSubset(targetSet, curEnv.captured, tree.srcPos)
      val sym = tree.symbol
      val cs = if sym.isConstructor then capturedVars(sym.owner) else CaptureSet.empty
      super.recheckApply(tree, pt).capturing(cs)

    override def recheck(tree: Tree, pt: Type = WildcardType)(using Context): Type =
      val saved = curEnv
      if pt.needsBox && !curEnv.isBoxed then
        curEnv = Env(NoSymbol, CaptureSet.Var(), true, curEnv)
      try
        val res = super.recheck(tree, pt)
        if curEnv.isOpen then assertSub(res.boxedCaptured, curEnv.captured)
        res
      finally curEnv = saved

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      super.checkUnit(unit)
      PostRefinerCheck.traverse(unit.tpdTree)

  end CaptureChecker

  inline val disallowGlobal = true

  def checkNotGlobal(tree: Tree, allArgs: Tree*)(using Context): Unit =
    if disallowGlobal then
      //println(i"checking $arg in $tree: ${arg.tpe.captureSet}")
      tree match
        case LambdaTypeTree(_, restpt) =>
          checkNotGlobal(restpt, allArgs*)
        case _ =>
          for ref <- tree.tpe.captureSet.elems do
            val isGlobal = ref match
              case ref: TypeRef => ref.isRootCapability
              case ref: TermRef => ref.prefix != NoPrefix && ref.symbol.hasAnnotation(defn.AbilityAnnot)
              case _ => false
            val what = if ref.isRootCapability then "universal" else "global"
            if isGlobal then
              val notAllowed = i" is not allowed to capture the $what capability $ref"
              def msg = tree match
                case tree: InferredTypeTree =>
                  i"""inferred type argument ${tree.tpe}$notAllowed
                      |
                      |The inferred arguments are: [$allArgs%, %]"""
                case _ => s"type argument$notAllowed"
              report.error(msg, tree.srcPos)

  object PostRefinerCheck extends TreeTraverser:
    def traverse(tree: Tree)(using Context) =
      tree match
        case tree1 @ TypeApply(fn, args) if disallowGlobal =>
          for arg <- args do checkNotGlobal(arg, args*)
        case _ =>
      traverseChildren(tree)

  def postRefinerCheck(tree: tpd.Tree)(using Context): Unit =
    PostRefinerCheck.traverse(tree)

end CheckCaptures
