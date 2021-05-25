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
import config.Printers.refinr
import ast.{tpd, untpd, Trees}
import NameKinds.{DocArtifactName, OuterSelectName, DefaultGetterName}
import Trees._
import scala.util.control.NonFatal
import typer.ErrorReporting._
import util.Spans.Span
import util.SimpleIdentitySet
import util.Chars.*
import Nullables._
import transform.*
import scala.collection.mutable
import reporting._
import ProtoTypes._
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions

class RefineTypes extends Phase, IdentityDenotTransformer:
  import RefineTypes.*
  import ast.tpd.*

  def phaseName: String = RefineTypes.name

  override def isTyper: Boolean = true

  def run(using Context): Unit =
    refinr.println(i"refine types of ${ctx.compilationUnit}")
    val refiner = newRefiner()
    val unit = ctx.compilationUnit
    val refineCtx = ctx
        .fresh
        .setMode(Mode.ImplicitsEnabled)
        .setTyper(refiner)
    val refinedTree = refiner.typedExpr(unit.tpdTree)(using refineCtx)
    if ctx.settings.Xprint.value.containsPhase(this) then
      report.echo(i"discarded result of $unit after refineTypes:\n\n$refinedTree")

  def preRefinePhase = this.prev.asInstanceOf[PreRefine]
  def thisPhase = this

  def newRefiner(): TypeRefiner = TypeRefiner()

  class TypeRefiner extends ReTyper:
    import ast.tpd.*

    override def newLikeThis: Typer = new TypeRefiner

    class RefineCompleter(val original: ValOrDefDef)(using Context) extends LazyType:
      def completeInCreationContext(symd: SymDenotation): Unit =
        val (paramss, paramFn) = original match
          case ddef: DefDef =>
            val paramss = ddef.paramss.nestedMap(_.symbol)
            (paramss, wrapMethodType(_: Type, paramss, isJava = false))
          case _: ValDef =>
            (Nil, (x: Type) => x)
        inContext(ctx.fresh.setOwner(symd.symbol).setTree(original)) {
          val rhsType = inferredResultType(original, symd.symbol, paramss, paramFn, WildcardType)
          typedAheadType(original.tpt, rhsType)
          symd.info = paramFn(rhsType)
        }

      def complete(symd: SymDenotation)(using Context): Unit = completeInCreationContext(symd)
    end RefineCompleter

    override def typedAhead(tree: untpd.Tree, typed: untpd.Tree => Tree)(using Context): Tree =
      tree.getAttachment(TypedAhead) match
        case Some(ttree) => ttree
        case none =>
          val ttree = typed(tree)
          tree.putAttachment(TypedAhead, ttree)
          ttree

    def resultNeedsRetyping(tree: untpd.ValOrDefDef)(using Context): Boolean =
      val sym = tree.symbol
      tree.tpt.isInstanceOf[InferredTypeTree[?]]
      && (sym.is(Private) || sym.owner.isTerm)
      && !sym.isOneOf(Param | JavaDefined)
      && !sym.isOneOf(FinalOrInline, butNot = Method | Mutable)
      && !sym.name.is(DefaultGetterName)

    override def index(trees: List[untpd.Tree])(using Context): Context =
      for case tree: ValOrDefDef <- trees.asInstanceOf[List[Tree]] do
        val sym = tree.symbol
        if tree.tpt.isInstanceOf[InferredTypeTree[?]]
          && (sym.is(Private) || sym.owner.isTerm)
          && !sym.isOneOf(Param | JavaDefined)
          && !sym.isOneOf(FinalOrInline, butNot = Method | Mutable)
          && !sym.name.is(DefaultGetterName)
        then
          tree.symbol.copySymDenotation().installAfter(thisPhase) // reset
          tree.symbol.copySymDenotation(
              info = RefineCompleter(tree),
              initFlags = tree.symbol.flags &~ Touched
          ).installAfter(preRefinePhase)
      ctx

    override def typedUnadapted(tree: untpd.Tree, pt: Type, locked: TypeVars)(using Context): Tree =
      trace(i"typed $tree, $pt", refinr, show = true) {
        tree.removeAttachment(TypedAhead) match
          case Some(ttree) => ttree
          case none =>
            tree match
              case _: untpd.TypedSplice
                  | _: untpd.Thicket
                  | _: EmptyValDef[?]
                  | _: untpd.TypeTree =>
                super.typedUnadapted(tree, pt, locked)
              case _ if tree.isType =>
                promote(tree)
              case _ =>
                super.typedUnadapted(tree, pt, locked)
      }

    override def typedSelect(tree: untpd.Select, pt: Type)(using Context): Tree =
      val Select(qual, name) = tree
      if name.is(OuterSelectName) then promote(tree)
      else
        val qual1 = withoutMode(Mode.Pattern)(typed(qual, AnySelectionProto))
        val qualType = qual1.tpe.widenIfUnstable
        val pre = maybeSkolemizePrefix(qualType, name)
        val mbr = qualType.findMember(name, pre,
            excluded = if tree.symbol.is(Private) then EmptyFlags else Private)
          .suchThat(tree.symbol ==)
        val ownType = qualType.select(name, mbr)
        untpd.cpy.Select(tree)(qual1, name).withType(ownType)

    override def typedTypeTree(tree: untpd.TypeTree, pt: Type)(using Context): TypeTree =
      if tree.isInstanceOf[InferredTypeTree[_]] && isFullyDefined(pt, ForceDegree.flipBottom) then
        tree.withType(pt)
      else
        promote(tree)

    override def typedTyped(tree: untpd.Typed, pt: Type)(using Context): Tree =
      val tpt1 = checkSimpleKinded(typedType(tree.tpt))
      val expr1 = tree.expr match
        case id: untpd.Ident if (ctx.mode is Mode.Pattern) && untpd.isVarPattern(id) && (id.name == nme.WILDCARD || id.name == nme.WILDCARD_STAR) =>
          tree.expr.withType(tpt1.tpe)
        case _ =>
          var pt1 = tpt1.tpe
          if pt1.isRepeatedParam then
            pt1 = pt1.translateFromRepeated(toArray = tree.expr.typeOpt.derivesFrom(defn.ArrayClass))
          typed(tree.expr, pt1)
      untpd.cpy.Typed(tree)(expr1, tpt1).withType(tree.typeOpt)

    private def resetTypeVars(tree: Tree)(using Context): (Tree, List[TypeVar]) = tree match
      case tree: TypeApply =>
        val isInferred = tree.args.forall {
          case arg: InferredTypeTree[?] =>
            arg.tpe match
              case tvar: TypeVar =>
                tvar.isInstantiated // test makes sure we do not reset typevars again in eta expanded closures
              case _ => false
          case _ => false
        }
        if isInferred then
          val args = tree.args
          val args1 = constrained(tree.fun.tpe.widen.asInstanceOf[TypeLambda], tree)._2
          for i <- args.indices do
            args1(i).tpe.asInstanceOf[TypeVar].link(args(i).tpe.asInstanceOf[TypeVar])
          (cpy.TypeApply(tree)(tree.fun, args1), args1.tpes.asInstanceOf[List[TypeVar]])
        else
          (tree, Nil)
      case Block(stats, closure: Closure) =>
        var tvars: List[TypeVar] = Nil
        val stats1 = stats.mapConserve {
          case stat: DefDef if stat.symbol == closure.meth.symbol =>
            val (rhs1, tvars1) = resetTypeVars(stat.rhs)
            tvars = tvars1
            cpy.DefDef(stat)(rhs = rhs1)
          case stat => stat
        }
        (cpy.Block(tree)(stats1, closure), tvars)
      case Block(Nil, expr) =>
        val (rhs1, tvars1) = resetTypeVars(expr)
        (cpy.Block(tree)(Nil, rhs1), tvars1)
      case _ =>
        (tree, Nil)
    end resetTypeVars

    override def typedTypeApply(tree: untpd.TypeApply, pt: Type)(using Context): Tree =
      if tree.symbol == defn.Predef_classOf then
        promote(tree)
      else
        val tree1 = resetTypeVars(tree.asInstanceOf[TypeApply])._1.asInstanceOf[TypeApply]
        super.typedTypeApply(tree1, pt)

    override def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(using Context): Tree =
      sym.ensureCompleted()
      if sym.isAnonymousFunction then
        val ddef0 = ddef.asInstanceOf[tpd.DefDef]
        val (rhs2, newTvars) = resetTypeVars(ddef0.rhs)
        val ddef1 = cpy.DefDef(ddef0)(rhs = rhs2)
        val bindsNestedTypeVar =
          newTvars.nonEmpty
          && sym.rawParamss.nestedExists(param =>
            param.info.existsPart({
              case tvar1: TypeVar => newTvars.exists(_.isLinked(tvar1))
              case _ => false
            }, stopAtStatic = true, forceLazy = false))
        if bindsNestedTypeVar then
          val nestedCtx = ctx.fresh.setNewTyperState()
          try inContext(nestedCtx) { super.typedDefDef(ddef1, sym) }
          finally nestedCtx.typerState.commit()
        else
          super.typedDefDef(ddef1, sym)
      else super.typedDefDef(ddef, sym)

    override def typedValDef(vdef: untpd.ValDef, sym: Symbol)(using Context): Tree =
      sym.ensureCompleted()
      super.typedValDef(vdef, sym)

    override def typedPackageDef(tree: untpd.PackageDef)(using Context): Tree =
      if tree.symbol == defn.StdLibPatchesPackage then
        promote(tree) // don't check stdlib patches, since their symbols were highjacked by stdlib classes
      else
        super.typedPackageDef(tree)
  end TypeRefiner

object RefineTypes:
  val name = "refineTypes"
end RefineTypes
