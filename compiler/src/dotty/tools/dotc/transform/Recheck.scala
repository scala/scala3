package dotty.tools
package dotc
package transform

import core.*
import Symbols.*, Contexts.*, Types.*, ContextOps.*, Decorators.*, SymDenotations.*
import Flags.*, SymUtils.*, NameKinds.*, Denotations.{Denotation, SingleDenotation}
import ast.*
import Names.Name
import Phases.Phase
import DenotTransformers.{DenotTransformer, IdentityDenotTransformer, SymTransformer}
import NamerOps.{methodType, linkConstructorParams}
import NullOpsDecorator.stripNull
import typer.ErrorReporting.err
import typer.ProtoTypes.*
import typer.TypeAssigner.seqLitType
import typer.ConstFold
import typer.ErrorReporting.{Addenda, NothingToAdd}
import NamerOps.methodType
import config.Printers.recheckr
import util.Property
import StdNames.nme
import reporting.trace
import annotation.constructorOnly
import cc.CaptureSet.IdempotentCaptRefMap
import annotation.tailrec

object Recheck:
  import tpd.*

  /** A flag used to indicate that a ParamAccessor has been temporarily made not-private
   *  Only used at the start of the Recheck phase, reset at its end.
   *  The flag repurposes the Scala2ModuleVar flag. No confusion is possible since
   *  Scala2ModuleVar cannot be also ParamAccessors.
   */
  val ResetPrivate = Scala2ModuleVar
  val ResetPrivateParamAccessor = ResetPrivate | ParamAccessor

  /** Attachment key for rechecked types of TypeTrees */
  val RecheckedType = Property.Key[Type]

  val addRecheckedTypes = new TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      val tree1 = super.transform(tree)
      tree.getAttachment(RecheckedType) match
        case Some(tpe) => tree1.withType(tpe)
        case None => tree1

  extension (sym: Symbol)(using Context)

    /** Update symbol's info to newInfo after `prevPhase`.
     *  Also update owner to newOwnerOrNull if it is not null.
     *  The update is valid until after Recheck. After that the symbol's denotation
     *  is reset to what it was before PreRecheck.
     */
    def updateInfo(prevPhase: DenotTransformer, newInfo: Type, newFlags: FlagSet = sym.flags, newOwner: Symbol = sym.owner): Unit =
      if (sym.info ne newInfo) || sym.flags != newFlags || (sym.maybeOwner ne newOwner)  then
        val flags = if newInfo.isInstanceOf[LazyType] then newFlags &~ Touched else newFlags
        sym.copySymDenotation(owner = newOwner, info = newInfo, initFlags = flags)
          .installAfter(prevPhase)

    /** Does symbol have a new denotation valid from phase.next that is different
     *  from the denotation it had before?
     */
    def isUpdatedAfter(phase: Phase) =
      val symd = sym.denot
      symd.validFor.firstPhaseId == phase.id + 1 && (sym.originDenotation ne symd)

  extension [T <: Tree](tree: T)

    /** Remember `tpe` as the type of `tree`, which might be different from the
     *  type stored in the tree itself, unless a type was already remembered for `tree`.
     */
    def rememberType(tpe: Type)(using Context): Unit =
      if !tree.hasAttachment(RecheckedType) then rememberTypeAlways(tpe)

    /** Remember `tpe` as the type of `tree`, which might be different from the
     *  type stored in the tree itself
     */
    def rememberTypeAlways(tpe: Type)(using Context): Unit =
      if tpe ne tree.knownType then tree.putAttachment(RecheckedType, tpe)

    /** The remembered type of the tree, or if none was installed, the original type */
    def knownType: Type =
      tree.attachmentOrElse(RecheckedType, tree.tpe)

    def hasRememberedType: Boolean = tree.hasAttachment(RecheckedType)

    def withKnownType(using Context): T = tree.getAttachment(RecheckedType) match
      case Some(tpe) => tree.withType(tpe).asInstanceOf[T]
      case None => tree

  /** Map ExprType => T to () ?=> T (and analogously for pure versions).
   *  Even though this phase runs after ElimByName, ExprTypes can still occur
   *  as by-name arguments of applied types. See note in doc comment for
   *  ElimByName phase. Test case is bynamefun.scala.
   */
  private def mapExprType(tp: Type)(using Context): Type = tp match
    case ExprType(rt) => defn.ByNameFunction(rt)
    case _ => tp

  /** Normalize `=> A` types to `() ?=> A` types
   *   - at the top level
   *   - in function and method parameter types
   *   - under annotations
   */
  def normalizeByName(tp: Type)(using Context): Type = tp.dealias match
    case tp: ExprType =>
      mapExprType(tp)
    case tp: PolyType =>
      tp.derivedLambdaType(resType = normalizeByName(tp.resType))
    case tp: MethodType =>
      tp.derivedLambdaType(
        paramInfos = tp.paramInfos.mapConserve(mapExprType),
        resType = normalizeByName(tp.resType))
    case tp @ RefinedType(parent, nme.apply, rinfo) if defn.isFunctionType(tp) =>
      tp.derivedRefinedType(parent, nme.apply, normalizeByName(rinfo))
    case tp @ defn.FunctionOf(pformals, restpe, isContextual) =>
      val pformals1 = pformals.mapConserve(mapExprType)
      val restpe1 = normalizeByName(restpe)
      if (pformals1 ne pformals) || (restpe1 ne restpe) then
        defn.FunctionOf(pformals1, restpe1, isContextual)
      else
        tp
    case tp @ AnnotatedType(parent, ann) =>
      tp.derivedAnnotatedType(normalizeByName(parent), ann)
    case _ =>
      tp
end Recheck

/** A base class that runs a simplified typer pass over an already re-typed program. The pass
 *  does not transform trees but returns instead the re-typed type of each tree as it is
 *  traversed. The Recheck phase must be directly preceded by a phase of type PreRecheck.
 */
abstract class Recheck extends Phase, SymTransformer:
  thisPhase =>

  import ast.tpd.*
  import Recheck.*

  /** The phase before rechecking, used to setup symbol infos. */
  def preRecheckPhase = this.prev.asInstanceOf[PreRecheck]

  /** The first phase that pepares for rechecking. This is usually preRecheckPhase
   *  but could also be before. Updated symbols will snap back to their
   *  denotations at firestPrepPhase after rechecking.
   */
  def firstPrepPhase: Phase = preRecheckPhase

  override def changesBaseTypes: Boolean = true

  override def isCheckable = false
    // TODO: investigate what goes wrong we Ycheck directly after rechecking.
    // One failing test is pos/i583a.scala

  /** Change denotation back to what it was before (pre-)rechecking` */
  def transformSym(symd: SymDenotation)(using Context): SymDenotation =
    val sym = symd.symbol
    def updatedAfter(p: Phase): Boolean =
      sym.isUpdatedAfter(p) || p != preRecheckPhase && updatedAfter(p.next)
    if updatedAfter(firstPrepPhase)
    then atPhase(firstPrepPhase)(sym.denot.copySymDenotation())
    else symd

  def run(using Context): Unit =
    val rechecker = newRechecker()
    rechecker.checkUnit(ctx.compilationUnit)
    rechecker.reset()

  override def runOn(units: List[CompilationUnit])(using runCtx: Context): List[CompilationUnit] =
    try super.runOn(units)
    finally preRecheckPhase.pastRecheck = true

  def newRechecker()(using Context): Rechecker

  /** The typechecker pass */
  class Rechecker(@constructorOnly ictx: Context):
    private val ta = ictx.typeAssigner

    /** If true, remember types of all tree nodes in attachments so that they
     *  can be retrieved with `knownType`
     */
    private val keepAllTypes = inContext(ictx) {
      ictx.settings.Xprint.value.containsPhase(thisPhase)
    }

    /** Should type of `tree` be kept in an attachment so that it can be retrieved with
     *  `knownType`? By default true only is `keepAllTypes` hold, but can be overridden.
     */
    def keepType(tree: Tree): Boolean = keepAllTypes

    /** A map from NamedTypes to the denotations they had before this phase.
     *  Needed so that we can `reset` them after this phase.
     */
    private val prevSelDenots = util.HashMap[NamedType, Denotation]()

    /** Reset all references in `prevSelDenots` to the denotations they had
     *  before this phase.
     */
    def reset()(using Context): Unit =
      for (ref, mbr) <- prevSelDenots.iterator do
        ref.withDenot(mbr)

    /** Constant-folded rechecked type `tp` of tree `tree` */
    protected def constFold(tree: Tree, tp: Type)(using Context): Type =
      val tree1 = tree.withType(tp)
      val tree2 = ConstFold(tree1)
      if tree2 ne tree1 then tree2.tpe else tp

    def recheckIdent(tree: Ident, pt: Type)(using Context): Type =
      tree.tpe

    def recheckSelect(tree: Select, pt: Type)(using Context): Type =
      val Select(qual, name) = tree
      val proto =
        if tree.symbol == defn.Any_asInstanceOf then WildcardType
        else AnySelectionProto
      recheckSelection(tree, recheck(qual, proto).widenIfUnstable, name, pt)

    def recheckSelection(tree: Select, qualType: Type, name: Name,
        sharpen: Denotation => Denotation)(using Context): Type =
      if name.is(OuterSelectName) then tree.tpe
      else
        //val pre = ta.maybeSkolemizePrefix(qualType, name)
        val mbr =
          sharpen(
            qualType.findMember(name, qualType,
              excluded = if tree.symbol.is(Private) then EmptyFlags else Private
          )).suchThat(tree.symbol == _)
        val newType = tree.tpe match
          case prevType: NamedType =>
            val prevDenot = prevType.denot
            val newType = qualType.select(name, mbr)
            if (newType eq prevType) && (mbr.info ne prevDenot.info) && !prevSelDenots.contains(prevType) then
              // remember previous denot of NamedType, so that it can be reset after this phase
              prevSelDenots(prevType) = prevDenot
            newType
          case _ =>
            qualType.select(name, mbr)
        constFold(tree, newType)
          //.showing(i"recheck select $qualType . $name : ${mbr.info} = $result")

    /** Keep the symbol of the `select` but re-infer its type */
    def recheckSelection(tree: Select, qualType: Type, name: Name, pt: Type)(using Context): Type =
      recheckSelection(tree, qualType, name, sharpen = identity[Denotation])

    def recheckBind(tree: Bind, pt: Type)(using Context): Type = tree match
      case Bind(name, body) =>
        recheck(body, pt)
        tree.symbol.namedType

    def recheckLabeled(tree: Labeled, pt: Type)(using Context): Type = tree match
      case Labeled(bind, expr) =>
        val (bindType: NamedType) = recheck(bind, pt): @unchecked
        val exprType = recheck(expr, defn.UnitType)
        bindType.symbol.info

    def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Type =
      val resType = recheck(tree.tpt)
      if tree.rhs.isEmpty then resType
      else recheck(tree.rhs, resType)

    def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Type =
      inContext(linkConstructorParams(sym).withOwner(sym)):
        val resType = recheck(tree.tpt)
        if tree.rhs.isEmpty || sym.isInlineMethod || sym.isEffectivelyErased
        then resType
        else recheck(tree.rhs, resType)

    def recheckTypeDef(tree: TypeDef, sym: Symbol)(using Context): Type =
      recheck(tree.rhs)
      sym.typeRef

    def recheckClassDef(tree: TypeDef, impl: Template, sym: ClassSymbol)(using Context): Type =
      recheck(impl.constr)
      impl.parentsOrDerived.foreach(recheck(_))
      recheck(impl.self)
      recheckStats(impl.body)
      sym.typeRef

    /** Assuming `formals` are parameters of a Java-defined method, remap Object
     *  to FromJavaObject since it got lost in ElimRepeated.
     *  NOTE: It seems this is no longer true, and `mapJavaArgs` is not needed.
     *  The invocation is currently disabled in recheckApply.
     */
    private def mapJavaArgs(formals: List[Type])(using Context): List[Type] =
      val tm = new TypeMap with IdempotentCaptRefMap:
        def apply(t: Type) =
          t match
            case t: TypeRef if t.symbol == defn.ObjectClass => defn.FromJavaObjectType
            case _ => mapOver(t)
      formals.mapConserve(tm)

    /** Hook for method type instantiation */
    protected def instantiate(mt: MethodType, argTypes: List[Type], sym: Symbol)(using Context): Type =
      mt.instantiate(argTypes)

    /** A hook to massage the type of an applied method; currently not overridden */
    protected def prepareFunction(funtpe: MethodType, meth: Symbol)(using Context): MethodType = funtpe

    def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      val funtpe0 = recheck(tree.fun)
      // reuse the tree's type on signature polymorphic methods, instead of using the (wrong) rechecked one
      val funtpe1 = if tree.fun.symbol.originalSignaturePolymorphic.exists then tree.fun.tpe else funtpe0
      funtpe1.widen match
        case fntpe1: MethodType =>
          val fntpe = prepareFunction(fntpe1, tree.fun.symbol)
          assert(fntpe.paramInfos.hasSameLengthAs(tree.args))
          val formals =
            if false && tree.symbol.is(JavaDefined) // see NOTE in mapJavaArgs
            then mapJavaArgs(fntpe.paramInfos)
            else fntpe.paramInfos
          def recheckArgs(args: List[Tree], formals: List[Type], prefs: List[ParamRef]): List[Type] = args match
            case arg :: args1 =>
              val argType = recheck(arg, normalizeByName(formals.head))
              val formals1 =
                if fntpe.isParamDependent
                then formals.tail.map(_.substParam(prefs.head, argType))
                else formals.tail
              argType :: recheckArgs(args1, formals1, prefs.tail)
            case Nil =>
              assert(formals.isEmpty)
              Nil
          val argTypes = recheckArgs(tree.args, formals, fntpe.paramRefs)
          constFold(tree, instantiate(fntpe, argTypes, tree.fun.symbol))
            //.showing(i"typed app $tree : $fntpe with ${tree.args}%, % : $argTypes%, % = $result")
        case tp =>
          assert(false, i"unexpected type of ${tree.fun}: $tp")

    def recheckTypeApply(tree: TypeApply, pt: Type)(using Context): Type =
      val funtpe = recheck(tree.fun)
      tree.fun.rememberType(funtpe) // remember type to support later bounds checks
      funtpe.widen match
        case fntpe: PolyType =>
          assert(fntpe.paramInfos.hasSameLengthAs(tree.args))
          val argTypes = tree.args.map(recheck(_))
          constFold(tree, fntpe.instantiate(argTypes))

    def recheckTyped(tree: Typed)(using Context): Type =
      val tptType = recheck(tree.tpt)
      recheck(tree.expr, tptType)
      tptType

    def recheckAssign(tree: Assign)(using Context): Type =
      val lhsType = recheck(tree.lhs, LhsProto)
      recheck(tree.rhs, lhsType.widen)
      defn.UnitType

    private def recheckBlock(stats: List[Tree], expr: Tree)(using Context): Type =
      recheckStats(stats)
      val exprType = recheck(expr)
      TypeOps.avoid(exprType, localSyms(stats).filterConserve(_.isTerm))

    def recheckBlock(tree: Block, pt: Type)(using Context): Type = tree match
      case Block(Nil, expr: Block) => recheckBlock(expr, pt)
      case Block((mdef : DefDef) :: Nil, closure: Closure) =>
        recheckClosureBlock(mdef, closure.withSpan(tree.span), pt)
      case Block(stats, expr) => recheckBlock(stats, expr)
        // The expected type `pt` is not propagated. Doing so would allow variables in the
        // expected type to contain references to local symbols of the block, so the
        // local symbols could escape that way.

    def recheckClosureBlock(mdef: DefDef, expr: Closure, pt: Type)(using Context): Type =
      recheckBlock(mdef :: Nil, expr)

    def recheckInlined(tree: Inlined, pt: Type)(using Context): Type =
      recheckBlock(tree.bindings, tree.expansion)(using inlineContext(tree))

    def recheckIf(tree: If, pt: Type)(using Context): Type =
      recheck(tree.cond, defn.BooleanType)
      recheck(tree.thenp, pt) | recheck(tree.elsep, pt)

    def recheckClosure(tree: Closure, pt: Type, forceDependent: Boolean = false)(using Context): Type =
      if tree.tpt.isEmpty then
        tree.meth.tpe.widen.toFunctionType(tree.meth.symbol.is(JavaDefined), alwaysDependent = forceDependent)
      else
        recheck(tree.tpt)

    def recheckMatch(tree: Match, pt: Type)(using Context): Type =
      val selectorType = recheck(tree.selector)
      val casesTypes = tree.cases.map(recheckCase(_, selectorType.widen, pt))
      TypeComparer.lub(casesTypes)

    def recheckCase(tree: CaseDef, selType: Type, pt: Type)(using Context): Type =
      recheck(tree.pat, selType)
      recheck(tree.guard, defn.BooleanType)
      recheck(tree.body, pt)

    def recheckReturn(tree: Return)(using Context): Type =
      // Avoid local pattern defined symbols in returns from matchResult blocks
      // that are inserted by the PatternMatcher transform.
      // For regular symbols in the case branch, we already avoid them by the rule
      // for blocks since a case branch is of the form `return[MatchResultN] { ... }`
      // For source-level returns from methods, there's nothing to avoid, since the
      // result type of a method with a return must be given explicitly.
      def avoidMap = new TypeOps.AvoidMap:
        def toAvoid(tp: NamedType) =
           tp.symbol.is(Case) && tp.symbol.owner.isContainedIn(ctx.owner)

      val rawType = recheck(tree.expr)
      val ownType = avoidMap(rawType)

      // The pattern matching translation, which runs before this phase
      // sometimes instantiates return types with singleton type alternatives
      // but the returned expression is widened. We compensate by widening the expected
      // type as well. See also `widenSkolems` in `checkConformsExpr` which fixes
      // a more general problem. It turns out that pattern matching returns
      // are not checked by Ycheck, that's why these problems were allowed to slip
      // through.
      def widened(tp: Type): Type = tp match
        case tp: SingletonType => tp.widen
        case tp: AndOrType => tp.derivedAndOrType(widened(tp.tp1), widened(tp.tp2))
        case tp @ AnnotatedType(tp1, ann) => tp.derivedAnnotatedType(widened(tp1), ann)
        case _ => tp
      checkConforms(ownType, widened(tree.from.symbol.returnProto), tree)
      defn.NothingType
    end recheckReturn

    def recheckWhileDo(tree: WhileDo)(using Context): Type =
      recheck(tree.cond, defn.BooleanType)
      recheck(tree.body, defn.UnitType)
      defn.UnitType

    def recheckTry(tree: Try, pt: Type)(using Context): Type =
      val bodyType = recheck(tree.expr, pt)
      val casesTypes = tree.cases.map(recheckCase(_, defn.ThrowableType, pt))
      val finalizerType = recheck(tree.finalizer, defn.UnitType)
      TypeComparer.lub(bodyType :: casesTypes)

    def recheckSeqLiteral(tree: SeqLiteral, pt: Type)(using Context): Type =
      val elemProto = pt.stripNull.elemType match
        case NoType => WildcardType
        case bounds: TypeBounds => WildcardType(bounds)
        case elemtp => elemtp
      val declaredElemType = recheck(tree.elemtpt)
      val elemTypes = tree.elems.map(recheck(_, elemProto))
      seqLitType(tree, TypeComparer.lub(declaredElemType :: elemTypes))

    def recheckTypeTree(tree: TypeTree)(using Context): Type =
      tree.knownType  // allows to install new types at Setup

    def recheckAnnotated(tree: Annotated)(using Context): Type =
      tree.tpe match
        case tp: AnnotatedType =>
          val argType = recheck(tree.arg)
          tp.derivedAnnotatedType(argType, tp.annot)

    def recheckAlternative(tree: Alternative, pt: Type)(using Context): Type =
      val altTypes = tree.trees.map(recheck(_, pt))
      TypeComparer.lub(altTypes)

    def recheckPackageDef(tree: PackageDef)(using Context): Type =
      recheckStats(tree.stats)
      NoType

    def recheckStats(stats: List[Tree])(using Context): Unit =
      @tailrec def traverse(stats: List[Tree])(using Context): Unit = stats match
        case (imp: Import) :: rest =>
          traverse(rest)(using ctx.importContext(imp, imp.symbol))
        case stat :: rest =>
          recheck(stat)
          traverse(rest)
        case _ =>
      traverse(stats)

    def recheckDef(tree: ValOrDefDef, sym: Symbol)(using Context): Type =
      inContext(ctx.localContext(tree, sym)) {
        tree match
          case tree: ValDef => recheckValDef(tree, sym)
          case tree: DefDef => recheckDefDef(tree, sym)
      }

    /** Recheck tree without adapting it, returning its new type.
     *  @param tree        the original tree
     *  @param pt          the expected result type
     */
    def recheckStart(tree: Tree, pt: Type = WildcardType)(using Context): Type =

      def recheckNamed(tree: NameTree, pt: Type)(using Context): Type =
        val sym = tree.symbol
        tree match
          case tree: Ident => recheckIdent(tree, pt)
          case tree: Select => recheckSelect(tree, pt)
          case tree: Bind => recheckBind(tree, pt)
          case tree: ValOrDefDef =>
            if tree.isEmpty then NoType
            else
              if sym.isUpdatedAfter(preRecheckPhase) then
                sym.ensureCompleted() // in this case the symbol's completer should recheck the right hand side
              else
                recheckDef(tree, sym)
              sym.termRef
          case tree: TypeDef =>
            // TODO: Should we allow for completers as for ValDefs or DefDefs?
            tree.rhs match
              case impl: Template =>
                recheckClassDef(tree, impl, sym.asClass)(using ctx.localContext(tree, sym))
              case _ =>
                recheckTypeDef(tree, sym)(using ctx.localContext(tree, sym))
          case tree: Labeled => recheckLabeled(tree, pt)

      def recheckUnnamed(tree: Tree, pt: Type): Type = tree match
        case tree: Apply => recheckApply(tree, pt)
        case tree: TypeApply => recheckTypeApply(tree, pt)
        case _: New | _: This | _: Super | _: Literal => tree.tpe
        case tree: Typed => recheckTyped(tree)
        case tree: Assign => recheckAssign(tree)
        case tree: Block => recheckBlock(tree, pt)
        case tree: If => recheckIf(tree, pt)
        case tree: Closure => recheckClosure(tree, pt)
        case tree: Match => recheckMatch(tree, pt)
        case tree: Return => recheckReturn(tree)
        case tree: WhileDo => recheckWhileDo(tree)
        case tree: Try => recheckTry(tree, pt)
        case tree: SeqLiteral => recheckSeqLiteral(tree, pt)
        case tree: Inlined => recheckInlined(tree, pt)
        case tree: TypeTree => recheckTypeTree(tree)
        case tree: Annotated => recheckAnnotated(tree)
        case tree: Alternative => recheckAlternative(tree, pt)
        case tree: PackageDef => recheckPackageDef(tree)
        case tree: Thicket => defn.NothingType
        case tree: Import => defn.NothingType

      tree match
        case tree: NameTree => recheckNamed(tree, pt)
        case tree => recheckUnnamed(tree, pt)
    end recheckStart

    /** Finish rechecking a tree node: check rechecked type against expected type
     *  and remember rechecked type in a tree attachment if required.
     *  @param tpe   the recheched type of `tree`
     *  @param tree  the rechecked tree
     *  @param pt    the expected type
     */
    def recheckFinish(tpe: Type, tree: Tree, pt: Type)(using Context): Type =
      val tpe1 = checkConforms(tpe, pt, tree)
      if keepType(tree) then tree.rememberType(tpe1)
      tpe1

    def recheck(tree: Tree, pt: Type = WildcardType)(using Context): Type =
      try recheckFinish(recheckStart(tree, pt), tree, pt)
      catch case ex: Exception =>
        println(i"error while rechecking $tree")
        throw ex

    /** Typing and previous transforms sometimes leaves skolem types in prefixes of
     *  NamedTypes in `expected` that do not match the `actual` Type. -Ycheck does
     *  not complain (need to find out why), but a full recheck does. We compensate
     *  by de-skolemizing everywhere in `expected` except when variance is negative.
     *  @return If `tp` contains SkolemTypes in covariant or invariant positions,
     *          the type where these SkolemTypes are mapped to their underlying type.
     *          Otherwise, `tp` itself
     */
    def widenSkolems(tp: Type)(using Context): Type =
      object widenSkolems extends TypeMap, IdempotentCaptRefMap:
        var didWiden: Boolean = false
        def apply(t: Type): Type = t match
          case t: SkolemType if variance >= 0 =>
            didWiden = true
            apply(t.underlying)
          case t: LazyRef => t
          case t @ AnnotatedType(t1, ann) => t.derivedAnnotatedType(apply(t1), ann)
          case _ => mapOver(t)
      val tp1 = widenSkolems(tp)
      if widenSkolems.didWiden then tp1 else tp

    /** If true, print info for some successful checkConforms operations (failing ones give
     *  an error message in any case).
     */
    private val debugSuccesses = false

    /** Check that widened types of `tpe` and `pt` are compatible. */
    def checkConforms(tpe: Type, pt: Type, tree: Tree)(using Context): Type = tree match
      case _: DefTree | EmptyTree | _: TypeTree => tpe
      case _ => checkConformsExpr(tpe.widenExpr, pt.widenExpr, tree)

    def isCompatible(actual: Type, expected: Type)(using Context): Boolean =
      actual <:< expected
      || expected.isRepeatedParam
          && isCompatible(actual,
              expected.translateFromRepeated(toArray = actual.isRef(defn.ArrayClass)))
      || {
        val widened = widenSkolems(expected)
        (widened ne expected) && isCompatible(actual, widened)
      }

    def checkConformsExpr(actual: Type, expected: Type, tree: Tree, addenda: Addenda = NothingToAdd)(using Context): Type =
      //println(i"check conforms $actual <:< $expected")
      if !isCompatible(actual, expected) then
        recheckr.println(i"conforms failed for ${tree}: $actual vs $expected")
        err.typeMismatch(tree.withType(actual), expected, addenda)
      actual

    def checkUnit(unit: CompilationUnit)(using Context): Unit =
      recheck(unit.tpdTree)

  end Rechecker

  /** Show tree with rechecked types instead of the types stored in the `.tpe` field */
  override def show(tree: untpd.Tree)(using Context): String =
    atPhase(thisPhase):
      withMode(Mode.Printing):
        super.show(addRecheckedTypes.transform(tree.asInstanceOf[tpd.Tree]))
end Recheck

/** A class that can be used to test basic rechecking without any customaization */
object TestRecheck:
  class Pre extends PreRecheck, IdentityDenotTransformer:
    override def isEnabled(using Context) = ctx.settings.YrecheckTest.value

class TestRecheck extends Recheck:
  def phaseName: String = "recheck"
  override def isEnabled(using Context) = ctx.settings.YrecheckTest.value
  def newRechecker()(using Context): Rechecker = Rechecker(ctx)


