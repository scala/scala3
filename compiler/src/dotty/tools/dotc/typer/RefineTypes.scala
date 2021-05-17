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
import typer.ProtoTypes._
import config.Printers.refiner
import ast.{tpd, untpd, Trees}
import core.NameKinds.{DocArtifactName, OuterSelectName}
import Trees._
import scala.util.control.NonFatal
import typer.ErrorReporting._
import util.Spans.Span
import Nullables._
import transform.*
import scala.collection.mutable
import util.Chars.*
import reporting._
import ProtoTypes._
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions


class RefineTypes extends Phase, IdentityDenotTransformer:
  import RefineTypes.*
  import ast.tpd.*

  def phaseName: String = RefineTypes.name

  override def isTyper: Boolean = true

  def run(using Context): Unit =
    println(i"refine types of ${ctx.compilationUnit}")
    val refiner = newRefiner()
    val refineCtx = ctx
        .fresh
        .setMode(Mode.ImplicitsEnabled)
        .setNewTyperState()
        .setTyper(refiner)
    ctx.typerState.constraint = OrderingConstraint.empty
    refiner.typedExpr(ctx.compilationUnit.tpdTree)(using refineCtx)

  def newRefiner(): TypeRefiner = TypeRefiner()

  class TypeRefiner extends ReTyper:
    import ast.tpd.*

    // don't check value classes after typer, as the constraint about constructors doesn't hold after transform
    override def checkDerivedValueClass(clazz: Symbol, stats: List[Tree])(using Context): Unit = ()

    /** Exclude all typevars that are referred to in a parameter of an enclosing closure */
    override def qualifyForInterpolation(tvars: TypeVars)(using Context): TypeVars =
      var qualifying = tvars
      val anonFuns = ctx.owner.ownersIterator.filter(_.isAnonymousFunction).toList
      if anonFuns.nonEmpty && false then
        val anonFunParamTypes = anonFuns.flatMap(_.rawParamss.flatten).map(_.info)
        qualifying.foreach (tvar =>
          if anonFunParamTypes.exists(formal =>
            formal.existsPart(tvar eq, stopAtStatic = true, forceLazy = false))
          then qualifying -= tvar
        )
      qualifying

    /** Exclude from double definition checks any erased symbols that were
     *  made `private` in phase `UnlinkErasedDecls`. These symbols will be removed
     *  completely in phase `Erasure` if they are defined in a currently compiled unit.
     */
    override def excludeFromDoubleDeclCheck(sym: Symbol)(using Context): Boolean =
      sym.isEffectivelyErased && sym.is(Private) && !sym.initial.is(Private)

    override def typedUnadapted(tree: untpd.Tree, pt: Type, locked: TypeVars)(using Context): Tree =
      trace(i"typed $tree, $pt", refiner, show = true) {
        tree match
          case _: untpd.TypedSplice | _: untpd.Thicket | _: EmptyValDef[?] =>
            super.typedUnadapted(tree, pt, locked)
          case _ if tree.isType =>
            promote(tree)
          case _ =>
            super.typedUnadapted(tree, pt, locked)
      }

    override def typedSelect(tree: untpd.Select, pt: Type)(using Context): Tree =
      val Select(qual, name) = tree
      val qual1 = withoutMode(Mode.Pattern)(typed(qual, AnySelectionProto))
      val qualType = qual1.tpe.widenIfUnstable
      val pre = maybeSkolemizePrefix(qualType, name)
      val mbr = qualType.findMember(name, pre).suchThat(tree.symbol ==)
      val ownType = qualType.select(name, mbr)
      untpd.cpy.Select(tree)(qual1, name).withType(ownType)

    override def typedPackageDef(tree: untpd.PackageDef)(using Context): Tree =
      if tree.symbol == defn.StdLibPatchesPackage then
        promote(tree) // don't check stdlib patches, since their symbols were highjacked by stdlib classes
      else
        super.typedPackageDef(tree)

    override def typedTypeApply(tree: untpd.TypeApply, pt: Type)(using Context): Tree =
      val isInferred = tree.args.forall {
        case arg: TypeVarBinder[?] => arg.typeOpt.isInstanceOf[TypeVar]
        case _ => false
      }
      if isInferred then
        var origin: Type = null
        for arg <- tree.args do
          assert(arg.isInstanceOf[untpd.TypeTree], arg)
          arg.typeOpt match
            case tvar: TypeVar =>
              if origin == null then origin = tvar.origin.binder
              else assert(tvar.origin.binder eq origin)
      if isInferred then
        val args1 = tree.args.asInstanceOf[List[tpd.Tree]]
        val tvars = args1.tpes.asInstanceOf[List[TypeVar]]
        for tvar <- tvars do
          tvar.resetInst(ctx.typerState)
          ctx.typerState.ownedVars += tvar
        val binder = tvars.head.origin.binder
        val added = ctx.typerState.constraint.ensureFresh(binder)
        if added ne binder then
          for i <- tvars.indices do
            tvars(i).setOrigin(added.paramRefs(i))
        val fun1 = typedExpr(tree.fun, PolyProto(args1, pt))
        //println(i"adding $added, $tree to ${ctx.typerState.constraint}")
        TypeComparer.addToConstraint(added, tvars)
        assignType(tree, fun1, args1)
      else
        super.typedTypeApply(tree, pt)

    //override def ensureNoLocalRefs(tree: Tree, pt: Type, localSyms: => List[Symbol])(using Context): Tree =
    //  tree

    //override def adapt(tree: Tree, pt: Type, locked: TypeVars, tryGadtHealing: Boolean)(using Context): Tree =
    //  tree

    //override def simplify(tree: Tree, pt: Type, locked: TypeVars)(using Context): tree.type = tree
  end TypeRefiner

object RefineTypes:
  val name = "refineTypes"
end RefineTypes
