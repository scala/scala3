package dotty.tools
package dotc
package transform

import core.*
import Symbols.*, Contexts.*, Types.*, ContextOps.*, Decorators.*, SymDenotations.*
import Flags.*, SymUtils.*, NameKinds.*
import ast.*
import Phases.Phase
import DenotTransformers.IdentityDenotTransformer
import NamerOps.{methodType, linkConstructorParams}
import NullOpsDecorator.stripNull
import typer.ErrorReporting.err
import typer.ProtoTypes.*
import typer.TypeAssigner.seqLitType
import typer.ConstFold
import NamerOps.methodType
import config.Printers.recheckr
import util.Property
import StdNames.nme
import reporting.trace

object Recheck:

  /** Attachment key for rechecked types of TypeTrees */
  private val RecheckedType = Property.Key[Type]

abstract class Recheck extends Phase, IdentityDenotTransformer:
  thisPhase =>

  import ast.tpd.*
  import Recheck.*

  def preRecheckPhase = this.prev.asInstanceOf[PreRecheck]

  override def isEnabled(using Context) = ctx.settings.Yrecheck.value
  override def changesBaseTypes: Boolean = true

  override def isCheckable = false
    // TODO: investigate what goes wrong we Ycheck directly after rechecking.
    // One failing test is pos/i583a.scala

  override def widenSkolems = true

  def run(using Context): Unit =
    val rechecker = newRechecker()
    rechecker.transformTypes.traverse(ctx.compilationUnit.tpdTree)
    rechecker.checkUnit(ctx.compilationUnit)

  def newRechecker()(using Context): Rechecker

  class Rechecker(ictx: Context):
    private val ta = ictx.typeAssigner
    private val keepTypes = inContext(ictx) {
      ictx.settings.Xprint.value.containsPhase(thisPhase)
    }

    extension (sym: Symbol) def updateInfo(newInfo: Type)(using Context): Unit =
      if sym.info ne newInfo then
        sym.copySymDenotation().installAfter(thisPhase) // reset
        sym.copySymDenotation(
            info = newInfo,
            initFlags =
              if newInfo.isInstanceOf[LazyType] then sym.flags &~ Touched
              else sym.flags
          ).installAfter(preRecheckPhase)

    extension (tpe: Type) def rememberFor(tree: Tree)(using Context): Unit =
      if (tpe ne tree.tpe) && !tree.hasAttachment(RecheckedType) then
        tree.putAttachment(RecheckedType, tpe)

    def knownType(tree: Tree) =
      tree.attachmentOrElse(RecheckedType, tree.tpe)

    def isUpdated(sym: Symbol)(using Context) =
      val symd = sym.denot
      symd.validFor.firstPhaseId == thisPhase.id && (sym.originDenotation ne symd)

    def transformType(tp: Type, inferred: Boolean, boxed: Boolean = false)(using Context): Type = tp

    object transformTypes extends TreeTraverser:

      // Substitute parameter symbols in `from` to paramRefs in corresponding
      // method or poly types `to`. We use a single BiTypeMap to do everything.
      class SubstParams(from: List[List[Symbol]], to: List[LambdaType])(using Context)
      extends DeepTypeMap, BiTypeMap:

        def apply(t: Type): Type = t match
          case t: NamedType =>
            val sym = t.symbol
            def outer(froms: List[List[Symbol]], tos: List[LambdaType]): Type =
              def inner(from: List[Symbol], to: List[ParamRef]): Type =
                if from.isEmpty then outer(froms.tail, tos.tail)
                else if sym eq from.head then to.head
                else inner(from.tail, to.tail)
              if tos.isEmpty then t
              else inner(froms.head, tos.head.paramRefs)
            outer(from, to)
          case _ =>
            mapOver(t)

        def inverse(t: Type): Type = t match
          case t: ParamRef =>
            def recur(from: List[LambdaType], to: List[List[Symbol]]): Type =
              if from.isEmpty then t
              else if t.binder eq from.head then to.head(t.paramNum).namedType
              else recur(from.tail, to.tail)
            recur(to, from)
          case _ =>
            mapOver(t)
      end SubstParams

      private def transformTT(tree: TypeTree, boxed: Boolean)(using Context) =
        transformType(tree.tpe, tree.isInstanceOf[InferredTypeTree], boxed).rememberFor(tree)

      def traverse(tree: Tree)(using Context) =
        tree match
          case tree @ ValDef(_, tpt: TypeTree, _) if tree.symbol.is(Mutable) =>
            transformTT(tpt, boxed = true)
            traverse(tree.rhs)
          case _ =>
            traverseChildren(tree)
        tree match
          case tree: TypeTree =>
            transformTT(tree, boxed = false)
          case tree: ValOrDefDef =>
            val sym = tree.symbol

            // replace an existing symbol info with inferred types
            def integrateRT(
                info: Type,                     // symbol info to replace
                psymss: List[List[Symbol]],     // the local (type and trem) parameter symbols corresponding to `info`
                prevPsymss: List[List[Symbol]], // the local parameter symbols seen previously in reverse order
                prevLambdas: List[LambdaType]   // the outer method and polytypes generated previously in reverse order
              ): Type =
              info match
                case mt: MethodOrPoly =>
                  val psyms = psymss.head
                  mt.companion(mt.paramNames)(
                    mt1 =>
                      if !psyms.exists(isUpdated) && !mt.isParamDependent && prevLambdas.isEmpty then
                        mt.paramInfos
                      else
                        val subst = SubstParams(psyms :: prevPsymss, mt1 :: prevLambdas)
                        psyms.map(psym => subst(psym.info).asInstanceOf[mt.PInfo]),
                    mt1 =>
                      integrateRT(mt.resType, psymss.tail, psyms :: prevPsymss, mt1 :: prevLambdas)
                  )
                case info: ExprType =>
                  info.derivedExprType(resType =
                    integrateRT(info.resType, psymss, prevPsymss, prevLambdas))
                case _ =>
                  val restp = knownType(tree.tpt)
                  if prevLambdas.isEmpty then restp
                  else SubstParams(prevPsymss, prevLambdas)(restp)

            if tree.tpt.hasAttachment(RecheckedType) && !sym.isConstructor then
              val newInfo = integrateRT(sym.info, sym.paramSymss, Nil, Nil)
                .showing(i"update info $sym: ${sym.info} --> $result", recheckr)
              if newInfo ne sym.info then
                val completer = new LazyType:
                  def complete(denot: SymDenotation)(using Context) =
                    denot.info = newInfo
                    recheckDef(tree, sym)
                sym.updateInfo(completer)
          case tree: Bind =>
            val sym = tree.symbol
            sym.updateInfo(transformType(sym.info, inferred = true))
          case _ =>
    end transformTypes

    def constFold(tree: Tree, tp: Type)(using Context): Type =
      val tree1 = tree.withType(tp)
      val tree2 = ConstFold(tree1)
      if tree2 ne tree1 then tree2.tpe else tp

    def recheckIdent(tree: Ident)(using Context): Type =
      tree.tpe

    /** Keep the symbol of the `select` but re-infer its type */
    def recheckSelect(tree: Select)(using Context): Type = tree match
      case Select(qual, name) =>
        val qualType = recheck(qual).widenIfUnstable
        if name.is(OuterSelectName) then tree.tpe
        else
          //val pre = ta.maybeSkolemizePrefix(qualType, name)
          val mbr = qualType.findMember(name, qualType,
              excluded = if tree.symbol.is(Private) then EmptyFlags else Private
            ).suchThat(tree.symbol ==)
          constFold(tree, qualType.select(name, mbr))
            //.showing(i"recheck select $qualType . $name : ${mbr.symbol.info} = $result")

    def recheckBind(tree: Bind, pt: Type)(using Context): Type = tree match
      case Bind(name, body) =>
        recheck(body, pt)
        val sym = tree.symbol
        if sym.isType then sym.typeRef else sym.info

    def recheckLabeled(tree: Labeled, pt: Type)(using Context): Type = tree match
      case Labeled(bind, expr) =>
        val bindType = recheck(bind, pt)
        val exprType = recheck(expr, defn.UnitType)
        bindType

    def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Unit =
      if !tree.rhs.isEmpty then recheckRHS(tree.rhs, sym.info, sym)

    def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Unit =
      val rhsCtx = linkConstructorParams(sym).withOwner(sym)
      if !tree.rhs.isEmpty && !sym.isInlineMethod && !sym.isEffectivelyErased then
        inContext(rhsCtx) { recheckRHS(tree.rhs, recheck(tree.tpt), sym) }

    def recheckRHS(tree: Tree, pt: Type, sym: Symbol)(using Context): Type =
      recheck(tree, pt)

    def recheckTypeDef(tree: TypeDef, sym: Symbol)(using Context): Type =
      recheck(tree.rhs)
      sym.typeRef

    def recheckClassDef(tree: TypeDef, impl: Template, sym: ClassSymbol)(using Context): Type =
      recheck(impl.constr)
      impl.parentsOrDerived.foreach(recheck(_))
      recheck(impl.self)
      recheckStats(impl.body)
      sym.typeRef

    // Need to remap Object to FromJavaObject since it got lost in ElimRepeated
    private def mapJavaArgs(formals: List[Type])(using Context): List[Type] =
      val tm = new TypeMap:
        def apply(t: Type) = t match
          case t: TypeRef if t.symbol == defn.ObjectClass => defn.FromJavaObjectType
          case _ => mapOver(t)
      formals.mapConserve(tm)

    /** Hook for method type instantiation
     */
    protected def instantiate(mt: MethodType, argTypes: List[Type], sym: Symbol)(using Context): Type =
      mt.instantiate(argTypes)

    def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      recheck(tree.fun).widen match
        case fntpe: MethodType =>
          assert(sameLength(fntpe.paramInfos, tree.args))
          val formals =
            if tree.symbol.is(JavaDefined) then mapJavaArgs(fntpe.paramInfos)
            else fntpe.paramInfos
          def recheckArgs(args: List[Tree], formals: List[Type], prefs: List[ParamRef]): List[Type] = args match
            case arg :: args1 =>
              val argType = recheck(arg, formals.head)
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

    def recheckTypeApply(tree: TypeApply, pt: Type)(using Context): Type =
      recheck(tree.fun).widen match
        case fntpe: PolyType =>
          assert(sameLength(fntpe.paramInfos, tree.args))
          val argTypes = tree.args.map(recheck(_))
          constFold(tree, fntpe.instantiate(argTypes))

    def recheckTyped(tree: Typed)(using Context): Type =
      val tptType = recheck(tree.tpt)
      recheck(tree.expr, tptType)
      tptType

    def recheckAssign(tree: Assign)(using Context): Type =
      val lhsType = recheck(tree.lhs)
      recheck(tree.rhs, lhsType.widen)
      defn.UnitType

    def recheckBlock(stats: List[Tree], expr: Tree, pt: Type)(using Context): Type =
      recheckStats(stats)
      val exprType = recheck(expr)
        // The expected type `pt` is not propagated. Doing so would allow variables in the
        // expected type to contain references to local symbols of the block, so the
        // local symbols could escape that way.
      TypeOps.avoid(exprType, localSyms(stats).filterConserve(_.isTerm))

    def recheckBlock(tree: Block, pt: Type)(using Context): Type =
      recheckBlock(tree.stats, tree.expr, pt)

    def recheckInlined(tree: Inlined, pt: Type)(using Context): Type =
      recheckBlock(tree.bindings, tree.expansion, pt)

    def recheckIf(tree: If, pt: Type)(using Context): Type =
      recheck(tree.cond, defn.BooleanType)
      recheck(tree.thenp, pt) | recheck(tree.elsep, pt)

    def recheckClosure(tree: Closure, pt: Type)(using Context): Type =
      if tree.tpt.isEmpty then
        tree.meth.tpe.widen.toFunctionType(tree.meth.symbol.is(JavaDefined))
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
      recheck(tree.expr, tree.from.symbol.returnProto)
      defn.NothingType

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
      knownType(tree)

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
      stats.foreach(recheck(_))

    def recheckDef(tree: ValOrDefDef, sym: Symbol)(using Context): Unit =
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
          case tree: Ident => recheckIdent(tree)
          case tree: Select => recheckSelect(tree)
          case tree: Bind => recheckBind(tree, pt)
          case tree: ValOrDefDef =>
            if tree.isEmpty then NoType
            else
              if isUpdated(sym) then sym.ensureCompleted()
              else recheckDef(tree, sym)
              sym.termRef
          case tree: TypeDef =>
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

      tree match
        case tree: NameTree => recheckNamed(tree, pt)
        case tree => recheckUnnamed(tree, pt)
    end recheckStart

    def recheckFinish(tpe: Type, tree: Tree, pt: Type)(using Context): Type =
      checkConforms(tpe, pt, tree)
      if keepTypes then tpe.rememberFor(tree)
      tpe

    def recheck(tree: Tree, pt: Type = WildcardType)(using Context): Type =
      trace(i"rechecking $tree with pt = $pt", recheckr, show = true) {
        try recheckFinish(recheckStart(tree, pt), tree, pt)
        catch case ex: Exception =>
          println(i"error while rechecking $tree")
          throw ex
      }

    private val debugSuccesses = false

    def checkConforms(tpe: Type, pt: Type, tree: Tree)(using Context): Unit = tree match
      case _: DefTree | EmptyTree | _: TypeTree | _: Closure =>
        // Don't report closure nodes, since their span is a point; wait instead
        // for enclosing block to preduce an error
      case _ =>
        val actual = tpe.widenExpr
        val expected = pt.widenExpr
        //println(i"check conforms $actual <:< $expected")
        val isCompatible =
          actual <:< expected
          || expected.isRepeatedParam
             && actual <:< expected.translateFromRepeated(toArray = tree.tpe.isRef(defn.ArrayClass))
        if !isCompatible then
          err.typeMismatch(tree.withType(tpe), expected)
        else if debugSuccesses then
          tree match
            case _: Ident =>
              println(i"SUCCESS $tree:\n${TypeComparer.explained(_.isSubType(actual, expected))}")
            case _ =>

    def checkUnit(unit: CompilationUnit)(using Context): Unit =
      recheck(unit.tpdTree)

  end Rechecker

  override def show(tree: untpd.Tree)(using Context): String =
    val addRecheckedTypes = new TreeMap:
      override def transform(tree: Tree)(using Context): Tree =
        val tree1 = super.transform(tree)
        tree.getAttachment(RecheckedType) match
          case Some(tpe) => tree1.withType(tpe)
          case None => tree1
    atPhase(thisPhase) {
      super.show(addRecheckedTypes.transform(tree.asInstanceOf[tpd.Tree]))
    }
end Recheck

class TestRecheck extends Recheck:
  def phaseName: String = "recheck"
  //override def isEnabled(using Context) = ctx.settings.YrefineTypes.value
  def newRechecker()(using Context): Rechecker = Rechecker(ctx)


