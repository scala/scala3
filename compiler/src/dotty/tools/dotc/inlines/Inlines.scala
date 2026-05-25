package dotty.tools
package dotc
package inlines

import ast.*, core.*
import Flags.*, Symbols.*, Types.*, Decorators.*, Constants.*, Contexts.*, TypeOps.*
import Names.Name
import StdNames.{str, tpnme, nme}
import NameOps.*
import typer.*
import NameKinds.BodyRetainerName
import SymDenotations.SymDenotation
import config.Printers.inlining
import ErrorReporting.errorTree
import dotty.tools.dotc.util.{SourceFile, SourcePosition, SrcPos}
import dotty.tools.dotc.transform.*
import dotty.tools.dotc.transform.MegaPhase
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import parsing.Parsers.Parser
import transform.{PostTyper, Inlining, CrossVersionChecks}
import staging.StagingLevel
import cc.CleanupRetains

import collection.mutable
import reporting.{NotConstant, trace}
import util.Spans.{Span, spanCoord}
import dotty.tools.dotc.core.Periods.PhaseId
import dotty.tools.dotc.util.chaining.*
import NameOps.expandedName
import dotty.tools.dotc.core.Annotations.ConcreteBodyAnnotation
import dotty.tools.dotc.core.Annotations.LazyBodyAnnotation
import dotty.tools.dotc.core.Scopes.EmptyScope
import dotty.tools.dotc.core.Scopes.MutableScope
import dotty.tools.dotc.reporting.OverrideError
import dotty.tools.dotc.typer.RefChecks.OverridingPairsChecker
import dotty.tools.dotc.typer.ErrorReporting.err
import dotty.tools.dotc.core.NameKinds.DefaultGetterName

/** Support for querying inlineable methods and for inlining calls to such methods */
object Inlines:
  import tpd.*

  /** An exception signalling that an inline info cannot be computed due to a
   *  cyclic reference. i14772.scala shows a case where this happens.
   */
  private[dotc] class MissingInlineInfo extends Exception

  /** `sym` is an inline method with a known body to inline.
   */
  def hasBodyToInline(sym: SymDenotation)(using Context): Boolean =
    (sym.isInlineMethod || sym.isInlineTrait) && sym.hasAnnotation(defn.BodyAnnot)

  /** The body to inline for method `sym`, or `EmptyTree` if none exists.
   *  @pre  hasBodyToInline(sym)
   */
  def bodyToInline(sym: SymDenotation)(using Context): Tree =
    if hasBodyToInline(sym) then
      sym.getAnnotation(defn.BodyAnnot).get.tree
        .tap: body =>
          for annot <- sym.getAnnotation(defn.NowarnAnnot) do
            val argPos = annot.argument(0).getOrElse(annot.tree).sourcePos
            val conf = annot.argumentConstantString(0).getOrElse("")
            ctx.run.nn.suppressions.registerNowarn(annot.tree.sourcePos, body.span)(conf, argPos)
    else
      EmptyTree

  def defsToInline(traitSym: SymDenotation)(using Context): List[Tree] =
    bodyToInline(traitSym) match
      case Block(defs, _) if traitSym.isInlineTrait => defs
      case _ => Nil

  /** Are we in an inline method body? */
  def inInlineMethod(using Context): Boolean =
    ctx.owner.ownersIterator.exists(_.isInlineMethod)

  def inInlineContext(using Context): Boolean =
    ctx.owner.ownersIterator.exists(sym => sym.isInlineMethod || sym.isInlineTrait)

  /** Can a call to method `meth` be inlined? */
  def isInlineable(meth: Symbol)(using Context): Boolean =
    meth.isInlineMethod && meth.hasAnnotation(defn.BodyAnnot) && !inInlineMethod

  def isInlineableFromInlineTrait(inlinedTraitSym: ClassSymbol, member: tpd.Tree)(using Context): Boolean =
    !(member.isInstanceOf[tpd.TypeDef] && inlinedTraitSym.typeParams.contains(member.symbol))
    && !member.symbol.isAllOf(Inline)
    // && !member.symbol.is(Deferred) // Also inline interfaces (see specialized-trait-collections-example.scala)

  /** Should call be inlined in this context? */
  def needsInlining(tree: Tree)(using Context): Boolean =
    def isInlineableInCtx =
      StagingLevel.level == 0
      && (
        ctx.phase == Phases.inliningPhase
        || (ctx.phase == Phases.typerPhase && needsTransparentInlining(tree))
        || (ctx.phase == Phases.specializeInlineTraitsPhase && !tree.symbol.is(Macro) && !(tree.symbol.isSpecializedTraitImplementationClass || tree.symbol.isSpecializedTraitInterface))
        || (ctx.phase == Phases.desugarSpecializedTraitsPhase && !tree.symbol.is(Macro) && (tree.symbol.isSpecializedTraitImplementationClass || tree.symbol.isSpecializedTraitInterface) )
      )
      && !ctx.typer.hasInliningErrors
      && !ctx.base.stopInlining
      // && !ctx.owner.ownersIterator.exists(_.isInlineTrait)

    tree match
      case Block(_, expr) =>
        needsInlining(expr)
      case tdef @ TypeDef(_, impl: Template) =>
        // !tdef.symbol.isInlineTrait &&
        impl.parents.map(symbolFromParent).exists(sym => sym.isInlineTrait) && isInlineableInCtx
      case _ =>
        def isUnapplyExpressionWithDummy: Boolean =
          // The first step of typing an `unapply` consists in typing the call
          // with a dummy argument (see Applications.typedUnApply). We delay the
          // inlining of this call.
          def rec(tree: Tree): Boolean = tree match
            case Apply(_, ProtoTypes.dummyTreeOfType(_) :: Nil) => true
            case Apply(fn, _) => rec(fn)
            case _ => false
          tree.symbol.name.isUnapplyName && rec(tree)
        isInlineable(tree.symbol) && !tree.tpe.widenTermRefExpr.isInstanceOf[MethodOrPoly] && isInlineableInCtx && !ctx.mode.is(Mode.NoInline) && !isUnapplyExpressionWithDummy

  private[dotc] def symbolFromParent(parent: Tree)(using Context): Symbol =
    if parent.symbol.isConstructor then parent.symbol.owner else parent.tpe.typeSymbol

  // Inline trait ancestors in linearization order.
  private def inlineTraitAncestors(cls: TypeDef)(using Context): List[Tree] = cls match {
    case tpd.TypeDef(_, tmpl: Template) =>
      val parentTrees: Map[Symbol, Tree] = tmpl.parents.map(par => symbolFromParent(par) -> par).toMap.filter(_._1.isInlineTrait)
      val ancestors: List[ClassSymbol] = cls.tpe.baseClasses.filter(sym => sym != cls.symbol && sym.isInlineTrait // && // TODO: Do we not need to stop if there is a non-inline trait somewhere in the hierarchy? It should block the inlining right?
            && !(cls.symbol.asClass.ownersIterator.toList.tail.exists(p => p.isInlineTrait)) // We can skip anything that would be inlined nested into an inline trait because it must be pruned out later                                                                                            
        )
      ancestors.flatMap(ancestor =>
        def baseTree =
          cls.tpe.baseType(ancestor) match
            case AppliedType(tycon, targs) =>
              Some(AppliedTypeTree(TypeTree(tycon), targs.map(TypeTree(_))))
            case tref: TypeRef =>
              Some(Ident(tref))
            case baseTpe =>
              report.error(s"unknown base type ${baseTpe.show} for ancestor ${ancestor.show} of ${cls.symbol.show}")
              None
        parentTrees.get(ancestor).orElse(baseTree.map(_.withSpan(cls.span)))
      ).flatMap { tree => 
        tree.tpe match {
          case Specialization(spec) if spec.hasSpecializedParams && !spec.isFullySpecialized => None // these can only exist in cases where we don't want to inline because:
                                                                                                   // 1) they will be pruned out later anyway and if we inline them we will create a loop (as in tests/pos/specialized-trait-inlining-causes-implementation-required-loop-bad.scala)
                                                                                                   // 2) they are covered by another specialized type parameter in inlining and therefore we will specialize later when the inline function is called
                                                                                                   //    tests/run/specialized-trait-inline-specialized-instance-with-specialization
                                                                                                   // 3) we already dealt with them in desugarSpecializedTraits
          case other => Some(tree)
        }
      }
    case _ =>
      Nil
  }

  private def needsTransparentInlining(tree: Tree)(using Context): Boolean =
    tree.symbol.is(Transparent)
    || ctx.mode.is(Mode.ForceInline)

  /** Try to inline a call to an inline method. Fail with error if the maximal
   *  inline depth is exceeded.
   *
   *  @param tree   The call to inline
   *  @return   An `Inlined` node that refers to the original call and the inlined bindings
   *            and body that replace it.
   */
  def inlineCall(tree: Tree)(using Context): Tree = ctx.profiler.onInlineCall(tree.symbol):

    /** Strip @retains annotations from inferred types in the call tree */
    val stripRetains = CleanupRetains()
    val stripper = new TreeTypeMap(
      treeMap = {
        case tree: InferredTypeTree =>
          val stripped = stripRetains(tree.tpe)
          if stripped ne tree.tpe then tree.withType(stripped)
          else tree
        case tree => tree
      }
    )

    val tree0 = stripper.transform(tree)

    if tree0.symbol.denot.exists
      && tree0.symbol.effectiveOwner == defn.CompiletimeTestingPackage.moduleClass
    then
      if (tree0.symbol == defn.CompiletimeTesting_typeChecks) return Intrinsics.typeChecks(tree0)
      if (tree0.symbol == defn.CompiletimeTesting_typeCheckErrors) return Intrinsics.typeCheckErrors(tree0)

    if ctx.isAfterTyper then
      // During typer we wait with cross version checks until PostTyper, in order
      // not to provoke cyclic references. See i16116 for a test case.
      CrossVersionChecks.checkRef(tree0.symbol, tree0.srcPos)

    if tree0.symbol.isConstructor then return tree // error already reported for the inline constructor definition

    /** Set the position of all trees logically contained in the expansion of
     *  inlined call `call` to the position of `call`. This transform is necessary
     *  when lifting bindings from the expansion to the outside of the call.
     */
    def liftFromInlined(call: Tree) = new TreeMap:
      override def transform(t: Tree)(using Context) =
        if call.span.exists then
          t match
            case t @ Inlined(_, Nil, expr) if t.inlinedFromOuterScope => expr
            case _ if t.isEmpty => t
            case _ => super.transform(t.withSpan(call.span))
        else t
    end liftFromInlined

    val bindings = new mutable.ListBuffer[Tree]

    /** Lift bindings around inline call or in its function part to
     *  the `bindings` buffer. This is done as an optimization to keep
     *  inline call expansions smaller.
     */
    def liftBindings(tree: Tree, liftPos: Tree => Tree): Tree = tree match {
      case Block(stats, expr) =>
        bindings ++= stats.map(liftPos)
        liftBindings(expr, liftPos)
      case tree @ Inlined(call, stats, expr) =>
        bindings ++= stats.map(liftPos)
        val lifter = liftFromInlined(call)
        cpy.Inlined(tree)(call, Nil, liftBindings(expr, liftFromInlined(call).transform(_)))
      case Apply(fn, args) =>
        cpy.Apply(tree)(liftBindings(fn, liftPos), args)
      case TypeApply(fn, args) =>
        fn.tpe.widenTermRefExpr match
          case tp: PolyType =>
            val targBounds = tp.instantiateParamInfos(args.map(_.tpe))
            for case (arg, bounds: TypeBounds) <- args.zip(targBounds) if !bounds.contains(arg.tpe) do
              val boundsStr =
                if bounds == TypeBounds.empty then " <: Any. Note that this type is higher-kinded."
                else bounds.show
              report.error(em"${arg.tpe} does not conform to bound$boundsStr", arg)
        cpy.TypeApply(tree)(liftBindings(fn, liftPos), args)
      case Select(qual, name) =>
        cpy.Select(tree)(liftBindings(qual, liftPos), name)
      case _ =>
        tree
    }

    // assertAllPositioned(tree0)   // debug
    val tree1 = liftBindings(tree0, identity)
    val tree2  =
      if bindings.nonEmpty then
        cpy.Block(tree0)(bindings.toList, inlineCall(tree1))
      else if enclosingInlineds.length < ctx.settings.XmaxInlines.value && !reachedInlinedTreesLimit then
        val body =
          try bodyToInline(tree0.symbol) // can typecheck the tree and thereby produce errors
          catch case _: MissingInlineInfo =>
            throw CyclicReference(ctx.owner)
        new InlineCall(tree0).expand(body)
      else
        ctx.base.stopInlining = true
        val (reason, setting) =
          if reachedInlinedTreesLimit then ("inlined trees", ctx.settings.XmaxInlinedTrees)
          else ("successive inlines", ctx.settings.XmaxInlines)
        errorTree(
          tree,
          em"""|Maximal number of $reason (${setting.value}) exceeded,
               |Maybe this is caused by a recursive inline method?
               |You can use ${setting.name} to change the limit.""",
          (tree :: enclosingInlineds).last.srcPos
        )
    if ctx.base.stopInlining && enclosingInlineds.isEmpty then
      ctx.base.stopInlining = false
        // we have completely backed out of the call that overflowed;
        // reset so that further inline calls can be expanded
    tree2
  end inlineCall

  private def updateFlagsFromInlinedParent(child: FlagSet, parent: FlagSet): FlagSet = 
    var updatedFlags = child
    // Parent needs to be initialised so child must also as initialisers have been inlined
    if (!parent.is(NoInits))
      updatedFlags &~= NoInits

    // Parent is impure; contaminates child as non-abstract methods have been inlined
    if (!parent.is(PureInterface))
      updatedFlags &~= PureInterface
    updatedFlags


  private def checkInnerClasses(tmpl: Template)(using Context) = 
    tmpl.body.foreach { 
      case innerClass: TypeDef if innerClass.symbol.isClass => report.error("Inline traits may not define inner classes or traits.", innerClass.srcPos)
      case _ =>
    }

  def checkAndTransformInlineTrait(inlineTrait: TypeDef)(using Context): TypeDef =
    val tpd.TypeDef(_, tmpl: Template) = inlineTrait: @unchecked
    checkInnerClasses(tmpl)
    val body1 = tmpl.body.flatMap {
      /* case innerClass: TypeDef if innerClass.symbol.isClass =>
         val newTrait = makeTraitFromInnerClass(innerClass)
         val newType = makeTypeFromInnerClass(inlineTrait.symbol, innerClass, newTrait.symbol)
         List(newTrait, newType) */
      case member: MemberDef =>
        List(member)
      case _ =>
        // Remove non-memberdefs, as they are normally placed into $init()
        Nil
    }
    val tmpl1 = cpy.Template(tmpl)(body = body1)
    cpy.TypeDef(inlineTrait)(rhs = tmpl1)
  end checkAndTransformInlineTrait


  private def checkInlineTraitOverrides(clsSym: ClassSymbol)(using Context) = 
    /* We need to enforce `override` modifier constraints
       here to ensure that the behaviour is the same as ordinary traits. The usual checks only apply
      in refChecks which is too late for us. */
    
    // TODO: This does cause some code duplication
    def checkInlineTraitOverride(member: Symbol, other: Symbol) =
      if !member.is(Override) && !other.is(Deferred) && member.owner == clsSym then
        report.error(
          OverrideError("needs `override` modifier", 
                        other.info,
                        member,
                        other,
                        NoType,
                        NoType),
          member.srcPos
        )
      else if member.owner != clsSym && other.owner != clsSym
          && !other.owner.derivesFrom(member.owner)
          && !(member.isAnyOverride || member.hasAnnotation(defn.UncheckedOverrideAnnot)) 
          && (!other.is(Deferred) || other.isAllOf(Given | HasDefault)) 
          && !member.is(Deferred) 
          && !other.name.is(DefaultGetterName) then
        
        report.error(
          OverrideError(
            s"${clsSym} inherits conflicting members:\n  "
            + err.infoString(other, clsSym.asClass.thisType, showLocation = true) + "  and\n  "
            + err.infoString(member, clsSym.thisType, showLocation = true)
            + "\n(Note: this can be resolved by declaring an override in " + clsSym + ".)",
            other.info,
            member,
            other,
            NoType,
            NoType)
          ,
          clsSym.srcPos
        )
    OverridingPairsChecker(clsSym, clsSym.thisType).checkAll(checkInlineTraitOverride) 

  def inlineParentInlineTraits(cls: Tree)(using Context): Tree =
    cls match {
      case cls @ tpd.TypeDef(_, impl: Template) =>
        checkInlineTraitOverrides(cls.symbol.asClass)
        val clsOverriddenSyms = cls.symbol.info.decls.toList.flatMap(_.allOverriddenSymbols).toSet
        val ancestors = inlineTraitAncestors(cls)
        val cycleFound = ancestors.exists { parent =>
          val parentSym = symbolFromParent(parent)
          val errorPos = if cls.symbol.ownersIterator.contains(parentSym) then Some(cls.srcPos) // Trying to inline into the tree which defines parentSym (need to catch this separately
                                                                                                // as need to catch it before we inline the second time to avoid tripping an assertion) 
                         else if ctx.inlineTraitState.inlineOrigins(cls.symbol).contains(parentSym) then 
                          val userPos = tpd.enclosingInlineds.last.srcPos // Select the user code that caused this error so we get two errors if there are two problematic inlines, not one
                          Some(userPos) // Trying to inline into the inlined body of parentSym not in the defn tree
                         else None // Fine
          errorPos.foreach(pos =>
            report.error(s"Inlining of inline traits looped. Tried to inline ${parentSym} into its own body.", pos)
          )
          errorPos.nonEmpty
        }

        if cycleFound then cls
        else {
          val newDefs = inContext(ctx.withOwner(cls.symbol)) {
            ancestors.foldLeft((List.empty[Tree], impl.body)){
              case ((inlineDefs, childDefs), parent) =>
                  val parentTraitInliner = InlineParentTrait(parent)
                  // Inline body
                  val overriddenSymbols = clsOverriddenSyms ++ inlineDefs.flatMap(_.symbol.allOverriddenSymbols)
                  // Need to put the new defs first because we process in linearization order to make overridees correct,
                  // but we want parent definitions to come first so that if child inline traits refer to values defined in a parent
                  // inline trait these are defined. 
                  val inlinedDefs1 = parentTraitInliner.expandDefs(overriddenSymbols) ::: inlineDefs 
                  cls.symbol.flags = updateFlagsFromInlinedParent(cls.symbol.flags, parent.symbol.flags)
                  
                  val childDefs1 = parentTraitInliner.adaptSuperCalls(childDefs)
                  (parentTraitInliner.adaptSuperCalls(inlinedDefs1), childDefs1)
            }
          }

          val newbody = newDefs._1 ::: newDefs._2
          val paramAccessors = newbody.filter(_.symbol.is(ParamAccessor))
          
          for pacc <- paramAccessors
              otherstat <- newbody if !otherstat.symbol.is(ParamAccessor) && otherstat.denot.matches(pacc.denot.asSingleDenotation)
          do report.error(s"Inlining of inline trait created name conflict on ${pacc.denot.name}. Constructor parameters of inline receivers may not collide with members of inline traits.", pacc.srcPos) 

          val impl1 = cpy.Template(impl)(body = newbody)

          cpy.TypeDef(cls)(rhs = impl1)
        }
      case _ =>
        cls
    }

  /** Try to inline a pattern with an inline unapply method. Fail with error if the maximal
   *  inline depth is exceeded.
   *
   *  @param fun The function of an Unapply node
   *  @return   An `Unapply` with a `fun` containing the inlined call to the unapply
   */
  def inlinedUnapplyFun(fun: tpd.Tree)(using Context): Tree =
    // We cannot inline the unapply directly, since the pattern matcher relies on unapply applications
    // as signposts what to do. On the other hand, we can do the inlining only in typer, not afterwards.
    // So the trick is to create a "wrapper" unapply in an anonymous class that has the inlined unapply
    // as its right hand side. The call to the wrapper unapply serves as the signpost for pattern matching.
    // After pattern matching, the anonymous class is removed in phase InlinePatterns with a beta reduction step.
    //
    // An inline unapply `P.unapply` in a pattern `P[...](using ...)(x1,x2,...)(using t1: T1, t2: T2, ...)` is transformed into
    // `{ class $anon { def unapply(s: S)(using t1: T1, t2: T2, ...): R = P.unapply[...](using ...)(s)(using t1, t2, ...) }; new $anon }.unapply(using y1,y2,...)`
    // and the call `P.unapply[...](using ...)(x1, x2, ...)(using t1, t2, ...)` is inlined.
    // This serves as a placeholder for the inlined body until the `patternMatcher` phase. After pattern matcher
    // transforms the patterns into terms, the `inlinePatterns` phase removes this anonymous class by β-reducing
    // the call to the `unapply`.

    val sym = fun.symbol

    val newUnapply = AnonClass(ctx.owner, List(defn.ObjectType), sym.coord) { cls =>
      // `fun` is a partially applied method that contains all type applications of the method.
      // The methodic type `fun.tpe.widen` is the type of the function starting from the scrutinee argument
      // and its type parameters are instantiated.
      val unapplyInfo = fun.tpe.widen
      val unapplySym = newSymbol(cls, sym.name.toTermName, Synthetic | Method, unapplyInfo, coord = sym.coord).entered

      val unapply = DefDef(unapplySym.asTerm, argss => fun.appliedToArgss(argss).withSpan(fun.span))

      if sym.is(Transparent) then
        // Inline the body and refine the type of the unapply method
        val inlinedBody = inlineCall(unapply.rhs)(using ctx.withOwner(unapplySym))
        val refinedResultType = inlinedBody.tpe.widen
        def refinedResult(info: Type): Type = info match
          case info: LambdaType => info.newLikeThis(info.paramNames, info.paramInfos, refinedResult(info.resultType))
          case _ => refinedResultType
        unapplySym.info = refinedResult(unapplyInfo)
        List(cpy.DefDef(unapply)(tpt = TypeTree(refinedResultType), rhs = inlinedBody))
      else
        List(unapply)
    }

    newUnapply.select(sym.name).withSpan(fun.span)
  end inlinedUnapplyFun

  /** For a retained inline method, another method that keeps track of
   *  the body that is kept at runtime. For instance, an inline method
   *
   *      inline override def f(x: T) = b
   *
   *  is complemented by the body retainer method
   *
   *      private def f$retainedBody(x: T) = f(x)
   *
   *  where the call `f(x)` is inline-expanded. This body is then transferred
   *  back to `f` at erasure, using method addRetainedInlineBodies.
   */
  def bodyRetainer(mdef: DefDef)(using Context): DefDef =
    val meth = mdef.symbol.asTerm

    val retainer = meth.copy(
      name = BodyRetainerName(meth.name),
      flags = (meth.flags &~ (Inline | Macro | Override | AbsOverride)) | Private,
      coord = mdef.rhs.span.startPos).asTerm.entered
    retainer.deriveTargetNameAnnotation(meth, name => BodyRetainerName(name.asTermName))
    DefDef(retainer, prefss =>
      inlineCall(
        ref(meth).appliedToArgss(prefss).withSpan(mdef.rhs.span.startPos))(
        using ctx.withOwner(retainer)))
    .showing(i"retainer for $meth: $result", inlining)

  /** Replace `Inlined` node by a block that contains its bindings and expansion */
  def dropInlined(inlined: Inlined)(using Context): Tree =
    val tree1 =
      if inlined.bindings.isEmpty then inlined.expansion
      else cpy.Block(inlined)(inlined.bindings, inlined.expansion)
    // Reposition in the outer most inlined call
    if (enclosingInlineds.nonEmpty) tree1 else reposition(tree1, inlined.span)

  def reposition(tree: Tree, callSpan: Span)(using Context): Tree =
    // Reference test tests/run/i4947b

    val curSource = ctx.compilationUnit.source

    // Tree copier that changes the source of all trees to `curSource`
    val cpyWithNewSource = new TypedTreeCopier {
      override protected def sourceFile(tree: tpd.Tree): SourceFile = curSource
      override protected val untpdCpy: untpd.UntypedTreeCopier = new untpd.UntypedTreeCopier {
        override protected def sourceFile(tree: untpd.Tree): SourceFile = curSource
      }
    }

    /** Removes all Inlined trees, replacing them with blocks.
     *  Repositions all trees directly inside an inlined expansion of a non empty call to the position of the call.
     *  Any tree directly inside an empty call (inlined in the inlined code) retains their position.
     *
     *  Until we implement JSR-45, we cannot represent in output positions in other source files.
     *  So, reposition inlined code from other files with the call position.
     */
    class Reposition extends TreeMap(cpyWithNewSource) {

      override def transform(tree: Tree)(using Context): Tree = {
        def fixSpan[T <: untpd.Tree](copied: T): T =
          copied.withSpan(if tree.source == curSource then tree.span else callSpan)
        def finalize(copied: untpd.Tree) =
          fixSpan(copied).withAttachmentsFrom(tree).withTypeUnchecked(tree.tpe)

        inContext(ctx.withSource(curSource)) {
          tree match
            case tree: Ident => finalize(untpd.Ident(tree.name)(using curSource))
            case tree: Literal => finalize(untpd.Literal(tree.const)(using curSource))
            case tree: This => finalize(untpd.This(tree.qual)(using curSource))
            case tree: JavaSeqLiteral => finalize(untpd.JavaSeqLiteral(transform(tree.elems), transform(tree.elemtpt))(using curSource))
            case tree: SeqLiteral => finalize(untpd.SeqLiteral(transform(tree.elems), transform(tree.elemtpt))(using curSource))
            case tree: Bind => finalize(untpd.Bind(tree.name, transform(tree.body))(using curSource))
            case tree: TypeTree => finalize(tpd.TypeTree(tree.tpe))
            case tree: DefTree => super.transform(tree).setDefTree
            case EmptyTree => tree
            case _ => fixSpan(super.transform(tree))
        }
      }
    }

    (new Reposition).transform(tree)
  end reposition

  /** Leave only a call trace consisting of
   *  - a reference to the top-level class from which the call was inlined,
   *  - the call's position
   *  in the call field of an Inlined node.
   *  The trace has enough info to completely reconstruct positions.
   *  Note: For macros it returns a Select and for other inline methods it returns an Ident (this distinction is only temporary to be able to run YCheckPositions)
   */
  def inlineCallTrace(callSym: Symbol, pos: SourcePosition)(using Context): Tree = {
    assert(ctx.source == pos.source)
    val topLevelCls = callSym.topLevelClass
    if (callSym.is(Macro)) ref(topLevelCls.owner).select(topLevelCls.name)(using ctx.withOwner(topLevelCls.owner)).withSpan(pos.span)
    else Ident(topLevelCls.typeRef).withSpan(pos.span)
  }

  private object Intrinsics:
    import dotty.tools.dotc.reporting.Diagnostic.Error
    private enum ErrorKind:
      case Parser, Typer

    private def compileForErrors(tree: Tree)(using Context): List[(ErrorKind, Error)] =
      assert(tree.symbol == defn.CompiletimeTesting_typeChecks || tree.symbol == defn.CompiletimeTesting_typeCheckErrors)
      def stripTyped(t: Tree): Tree = t match {
        case Typed(t2, _) => stripTyped(t2)
        case Block(Nil, t2) => stripTyped(t2)
        case Inlined(_, Nil, t2) => stripTyped(t2)
        case _ => t
      }

      val Apply(_, codeArg :: Nil) = tree: @unchecked
      val codeArg1 = stripTyped(codeArg.underlying)
      val underlyingCodeArg =
        if Inlines.isInlineable(codeArg1.symbol) then stripTyped(Inlines.inlineCall(codeArg1))
        else codeArg1

      // We should not be rewriting tested strings
      val noRewriteSettings = ctx.settings.rewrite.updateIn(ctx.settingsState.reinitializedCopy(), None)

      class MegaPhaseWithCustomPhaseId(miniPhases: Array[MiniPhase], startId: PhaseId, endId: PhaseId)
        extends MegaPhase(miniPhases) {
        override def start: Int = startId
        override def end: Int = endId
      }

      // Let's reconstruct necessary transform MegaPhases, without anything
      // that could cause problems here (like `CrossVersionChecks`).
      // The individiual lists here should line up with Compiler.scala, i.e
      // separate chunks there should also be kept separate here.
      // For now we create a single MegaPhase, since there does not seem to
      // be any important checks later (e.g. ForwardDepChecks could be applicable here,
      // but the equivalent is also not run in the scala 2's `ctx.typechecks`,
      // so let's leave it out for now).
      lazy val reconstructedTransformPhases =
        val transformPhases: List[List[(Class[?], () => MiniPhase)]] = List(
          List(
            (classOf[InlineVals], () => new InlineVals),
            (classOf[ElimRepeated], () => new ElimRepeated),
            (classOf[RefChecks], () => new RefChecks),
          ),
        )

        transformPhases.flatMap( (megaPhaseList: List[(Class[?], () => MiniPhase)]) =>
          val (newMegaPhasePhases, phaseIds) =
            megaPhaseList.flatMap {
              case (filteredPhaseClass, miniphaseConstructor) =>
                ctx.base.phases
                  .find(phase => filteredPhaseClass.isInstance(phase))
                  .map(phase => (miniphaseConstructor(), phase.id))
            }
            .unzip
          if newMegaPhasePhases.isEmpty then None
          else Some(MegaPhaseWithCustomPhaseId(newMegaPhasePhases.toArray, phaseIds.head, phaseIds.last))
        )

      ConstFold(underlyingCodeArg).tpe.widenTermRefExpr match {
        case ConstantType(Constant(code: String)) =>
          val unitName = "tasty-reflect"
          val source2 = SourceFile.virtual(unitName, code)
          def compilationUnits(untpdTree: untpd.Tree, tpdTree: Tree): List[CompilationUnit] =
            val compilationUnit = CompilationUnit(unitName, code)
            compilationUnit.tpdTree = tpdTree
            compilationUnit.untpdTree = untpdTree
            List(compilationUnit)
          // We need a dummy owner, as the actual one does not have a computed denotation yet,
          // but might be inspected in a transform phase, leading to cyclic errors
          val dummyOwner = newSymbol(ctx.owner, "$dummySymbol$".toTermName, Private, defn.AnyType, NoSymbol)
          val newContext =
            ctx.fresh
            .setSettings(noRewriteSettings)
            .setNewTyperState()
            .setTyper(new Typer(ctx.nestingLevel + 1))
            .setSource(source2)
            .withOwner(dummyOwner)

          inContext(newContext) {
            def noErrors = ctx.reporter.allErrors.isEmpty
            val untpdTree = new Parser(source2).block()
            if !noErrors then
              ctx.reporter.allErrors.map((ErrorKind.Parser, _))
            else
              val tpdTree1 = ctx.typer.typed(untpdTree)
              ctx.base.postTyperPhase match
                case postTyper: PostTyper if noErrors =>
                  val tpdTree2 =
                    atPhase(postTyper) { postTyper.runOn(compilationUnits(untpdTree, tpdTree1)).head.tpdTree }
                  ctx.base.setRootTreePhase match
                    case setRootTree if noErrors => // might be noPhase, if -Yretain-trees is not used
                      val tpdTree3 =
                        atPhase(setRootTree)(setRootTree.runOn(compilationUnits(untpdTree, tpdTree2)).head.tpdTree)
                      ctx.base.inliningPhase match
                        case inlining: Inlining if noErrors =>
                          val tpdTree4 = atPhase(inlining) { inlining.newTransformer.transform(tpdTree3) }
                          if noErrors && reconstructedTransformPhases.nonEmpty then
                            var transformTree = tpdTree4
                            for phase <- reconstructedTransformPhases do
                              if noErrors then
                                transformTree = atPhase(phase.end + 1)(phase.transformUnit(transformTree))
                        case _ =>
                    case _ =>
                case _ =>
              ctx.reporter.allErrors.map((ErrorKind.Typer, _))
          }
        case t =>
          report.error(em"argument to compileError must be a statically known String but was: $codeArg", codeArg1.srcPos)
          Nil
      }

    private def packError(kind: ErrorKind, error: Error)(using Context): Tree =
      def lit(x: Any) = Literal(Constant(x))
      val constructor: Tree = ref(defn.CompiletimeTesting_Error_apply)
      val parserErrorKind: Tree = ref(defn.CompiletimeTesting_ErrorKind_Parser)
      val typerErrorKind: Tree = ref(defn.CompiletimeTesting_ErrorKind_Typer)

      constructor.appliedTo(
        lit(error.message),
        lit(error.pos.lineContent.reverse.dropWhile("\n ".contains).reverse),
        lit(error.pos.column),
        if kind == ErrorKind.Parser then parserErrorKind else typerErrorKind)

    private def packErrors(errors: List[(ErrorKind, Error)], pos: SrcPos)(using Context): Tree =
      val individualErrors: List[Tree] = errors.map(packError)
      val errorTpt = ref(defn.CompiletimeTesting_ErrorClass).withSpan(pos.span)
      mkList(individualErrors, errorTpt)

    /** Expand call to scala.compiletime.testing.typeChecks */
    def typeChecks(tree: Tree)(using Context): Tree =
      val errors = compileForErrors(tree)
      Literal(Constant(errors.isEmpty)).withSpan(tree.span)

    /** Expand call to scala.compiletime.testing.typeCheckErrors */
    def typeCheckErrors(tree: Tree)(using Context): Tree =
      val errors = compileForErrors(tree)
      packErrors(errors, tree)

    /** Expand call to scala.compiletime.codeOf */
    def codeOf(arg: Tree, pos: SrcPos)(using Context): Tree =
      Literal(Constant(arg.show(using ctx.withoutColors))).withSpan(pos.span)
  end Intrinsics

  /** Produces an inlined version of `call` via its `inlined` method.
   *
   *  @param  call         the original call to an inlineable method
   *  @param  rhsToInline  the body of the inlineable method that replaces the call.
   */
  private class InlineCall(call: tpd.Tree)(using Context) extends Inliner(call):
    import tpd.*
    import Inlines.*

    /** The Inlined node representing the inlined call */
    def expand(rhsToInline: Tree): Tree =

      // Special handling of `requireConst` and `codeOf`
      callValueArgss match
        case (arg :: Nil) :: Nil =>
          if inlinedMethod == defn.Compiletime_requireConst then
            arg match
              case ConstantValue(_) | Inlined(_, Nil, Typed(ConstantValue(_), _)) => // ok
              case _ => report.error(em"expected a constant value but found: $arg", arg.srcPos)
            return unitLiteral.withSpan(call.span)
          else if inlinedMethod == defn.Compiletime_codeOf then
            return Intrinsics.codeOf(arg, call.srcPos)
        case _ =>

      // Special handling of `constValue[T]`, `constValueOpt[T]`, `constValueTuple[T]`, `summonInline[T]` and `summonAll[T]`
      if callTypeArgs.length == 1 then

        def constValueOrError(tpe: Type): Tree =
          val constVal = tryConstValue(tpe)
          if constVal.isEmpty then
            val msg = NotConstant("cannot take constValue", tpe)
            report.error(msg, callTypeArgs.head.srcPos)
            ref(defn.Predef_undefined).withSpan(callTypeArgs.head.span).withType(ErrorType(msg))
          else
            constVal

        def searchImplicitOrError(tpe: Type): Tree =
          val evTyper = new Typer(ctx.nestingLevel + 1)
          val evCtx = ctx.fresh.setTyper(evTyper)
          inContext(evCtx) {
            val evidence = evTyper.inferImplicitArg(tpe, callTypeArgs.head.span)
            evidence.tpe match
              case fail: Implicits.SearchFailureType =>
                errorTree(call, evTyper.missingArgMsg(evidence, tpe, ""))
              case _ =>
                evidence
          }

        def unrollTupleTypes(tpe: Type): Option[List[Type]] = tpe.dealias match
          case AppliedType(tycon, args) if defn.isTupleClass(tycon.typeSymbol) =>
            Some(args)
          case AppliedType(tycon, head :: tail :: Nil) if tycon.isRef(defn.PairClass) =>
            unrollTupleTypes(tail).map(head :: _)
          case tpe: TermRef if tpe.symbol == defn.EmptyTupleModule =>
            Some(Nil)
          case tpe: AppliedType if tpe.isMatchAlias =>
            unrollTupleTypes(tpe.tryNormalize)
          case _ =>
            None

        if (inlinedMethod == defn.Compiletime_constValue) {
          return constValueOrError(callTypeArgs.head.tpe)
        }
        else if (inlinedMethod == defn.Compiletime_constValueOpt) {
          val constVal = tryConstValue(callTypeArgs.head.tpe)
          return (
            if (constVal.isEmpty) ref(defn.NoneModule.termRef)
            else New(defn.SomeClass.typeRef.appliedTo(constVal.tpe), constVal :: Nil)
          )
        }
        else if (inlinedMethod == defn.Compiletime_constValueTuple) {
          unrollTupleTypes(callTypeArgs.head.tpe) match
            case Some(types) =>
              val constants = types.map(constValueOrError)
              return Typed(tpd.tupleTree(constants), TypeTree(callTypeArgs.head.tpe)).withSpan(call.span)
            case _ =>
              return errorTree(call, em"Tuple element types must be known at compile time")
        }
        else if (inlinedMethod == defn.Compiletime_summonInline) {
          return searchImplicitOrError(callTypeArgs.head.tpe)
        }
        else if (inlinedMethod == defn.Compiletime_summonAll) {
          unrollTupleTypes(callTypeArgs.head.tpe) match
            case Some(types) =>
              val implicits = types.map(searchImplicitOrError)
              return Typed(tpd.tupleTree(implicits), TypeTree(callTypeArgs.head.tpe)).withSpan(call.span)
            case _ =>
              return errorTree(call, em"Tuple element types must be known at compile time")
        }
      end if

      val (bindings, expansion) = super.inlined(rhsToInline)

      // Take care that only argument bindings go into `bindings`, since positions are
      // different for bindings from arguments and bindings from body.
      val inlined = tpd.Inlined(call, bindings, expansion)

      val hasOpaquesInResultFromCallWithTransparentContext =
        val owners = call.symbol.ownersIterator.toSet
        call.tpe.widenTermRefExpr.existsPart(
          part => part.typeSymbol.is(Opaque) && owners.contains(part.typeSymbol.owner)
        )

      /** Remap ThisType nodes that are incorrect in the inlined context.
       * Incorrect ThisType nodes can cause unwanted opaque type dealiasing later.
       * E.g. if inlined in a `<root>.Foo` package (but outside of <root>.Foo.Bar object) we will map
       *   `TermRef(ThisType(TypeRef(ThisType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object Foo),Bar$)),MyOpaque$)),one)`
       * into
       *   `TermRef(TermRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object Foo),object Bar),object MyOpaque),val one)`
       * See test i13461-d
       */
      def fixThisTypeModuleClassReferences(tpe: Type): Type =
        val owners = ctx.owner.ownersIterator.toSet
        TreeTypeMap(
          typeMap = new TypeMap:
            override def stopAt = StopAt.Package
            def apply(t: Type) =
              t match
                case ThisType(tref @ TypeRef(prefix, _)) if tref.symbol.flags.is(Module) && !owners.contains(tref.symbol) =>
                  TermRef(apply(prefix), tref.symbol.companionModule)
                case _ => mapOver(t)
        ).typeMap(tpe)

      if !hasOpaqueProxies && !hasOpaquesInResultFromCallWithTransparentContext then inlined
      else
        val (target, forceCast) =
          if inlinedMethod.is(Transparent) then
            val unpacked = unpackProxiesFromResultType(inlined)
            val withAdjustedThisTypes = if call.symbol.is(Macro) then fixThisTypeModuleClassReferences(unpacked) else unpacked
            (call.tpe & withAdjustedThisTypes, withAdjustedThisTypes != unpacked)
          else (call.tpe, false)
        // `target` might contain a method reference, which is an invalid cast target. Use its return type instead.
        // see https://github.com/scala/scala3/issues/25091
        val resultType = target.widenIfUnstable
        if forceCast then
          // we need to force the cast for issues with ThisTypes, as ensureConforms will just
          // check subtyping and then choose not to cast, leaving the previous, incorrect type
          inlined.cast(resultType)
        else if !(inlined.tpe <:< target) then
          // Make sure that the sealing with the declared type
          // is type correct. Without it we might get problems since the
          // expression's type is the opaque alias but the call's type is
          // the opaque type itself. An example is in pos/opaque-inline1.scala.
          //
          // Here we can't just use `inlined.ensureConforms(resultType)`:
          // `target.widenIfUnstable` is an upper approximation of `target`,
          // so a tree may conform to it while still not conforming to `target`.
          // This can happen when widening drops path-/prefix-sensitive information
          // (e.g. projected opaque-proxy types).
          // We check conformance against the original `target`, but cast to the
          // widened type to avoid NoType issues at erasure (see #25091, #25417).
          inlined.cast(resultType)
        else
          inlined
    end expand
  end InlineCall

  private class InlineParentTrait(parent: tpd.Tree)(using Context) extends Inliner(parent):
    import tpd._
    import Inlines.*

    private val parentSym = symbolFromParent(parent)
    private val paramAccessorsMapper = ParamAccessorsMapper()

    private val child = ctx.owner
    private val childThisType = ctx.owner.thisType
    private val childThisTree = This(ctx.owner.asClass).withSpan(parent.span)

    def inlinedSelfType =
      inlinerTypeMap(parentSym.asClass.classDenot.givenSelfType)

    def expandDefs(overriddenDecls: Set[Symbol]): List[Tree] =
      paramAccessorsMapper.registerParamValuesOf(parent)
      val stats = Inlines.defsToInline(parentSym).filterNot(stat => overriddenDecls.contains(stat.symbol) && stat.symbol.is(Deferred))

      val stats1 = stats.map{  // Private symbols must be entered before the RHSs are inlined
        case member: MemberDef => Left((member, inlinedSym(member.symbol, overriddenDecls)))
        case stat => Right(stat)
      }
      ctx.owner.info = ctx.owner.asClass.classInfo.integrateOpaqueMembers
      stats1.map{
        case Left((tree, inlinedSym)) => expandStat(tree, inlinedSym)
        case Right(tree) => inlinedRhs(tree)
      }
    end expandDefs

    def adaptSuperCalls(defs: List[Tree]) = 
      val ttmap = TreeTypeMap(treeMap = {
        // We go through all ancestor inline traits so eventually we will find the one with matching parentSym
        case sel@Select(Super(qual, mix), name) if sel.symbol.owner == parentSym =>
            // At that point either the method is overridden so needs mangling (and we just copied and mangled it in this inlining phase), 
            // or not, in which case call directly by original name. In both cases we are calling the method resulting from inlining, on the
            // inline receiver class.
            Select(This(ctx.owner.asClass), paramAccessorsMapper.getParamAccessorName(sel.symbol.owner, name).getOrElse(name))
        case tree => tree
      })
      defs.map(ttmap(_))

    protected class InlineTraitTypeMap extends InlinerTypeMap {
      override def apply(t: Type) = super.apply(t) match {
        case t: ThisType if t.cls == parentSym => childThisType
        case t => mapOver(t)
      }
    }

    protected class InlineTraitTreeMap extends InlinerTreeMap {
      override def apply(tree: Tree) = super.apply(tree) match {
        case tree: This if tree.symbol == parentSym =>
          Inlined(EmptyTree, Nil, childThisTree).withSpan(parent.span)
        case tree: This =>
          tree.tpe match {
            case thisTpe: ThisType if thisTpe.cls.isInlineTrait =>
              integrate(This(ctx.owner.asClass).withSpan(parent.span), thisTpe.cls)
            case _ =>
              tree
          }
        case sel@Select(Super(qual, mix), name) =>
          if sel.symbol.owner.isInlineTrait then   // We need to leave this intact so that adaptSuperCalls can redirect it later
            sel
          else
            report.error("Inline traits may not contain superclass references to classes or non-inline traits.", sel.srcPos)
            sel
        case sel@Select(qual, name) =>
          inContext(ctx.withSource(tree.source)) { // Need to ensure we preserve the fact that this Select was inlined
                                                   // potentially from a different file. Recreating it discards that info.
                                                   // See: inline-trait-multiple-stages-generic-defs.
            paramAccessorsMapper.getParamAccessorName(qual.symbol, name) match {
                case Some(newName) => Select(this(qual), newName).withSpan(parent.span)
                case None => Select(this(qual), name)
            }
          }
        case tree =>
          tree
      }
    }

    override protected val inlinerTypeMap: InlinerTypeMap = InlineTraitTypeMap()
    override protected val inlinerTreeMap: InlinerTreeMap = InlineTraitTreeMap()

    override protected def inlineCopier: tpd.TreeCopier = new TypedTreeCopier() {
      // FIXME it feels weird... Is this correct?
      override def Apply(tree: Tree)(fun: Tree, args: List[Tree])(using Context): Apply =
        untpd.cpy.Apply(tree)(fun, args).withTypeUnchecked(tree.tpe)
    }

    override protected def computeThisBindings(): Unit = ()
    override protected def canElideThis(tpe: ThisType): Boolean = true

    override protected def inlineCtx(inlineTyper: InlineTyper)(using Context): Context =
      ctx.fresh.setTyper(inlineTyper).setNewScope

    extension (sym: Symbol)
      private def isTermParamAccessor: Boolean = !sym.isType && sym.is(ParamAccessor)

    private def expandStat(stat: tpd.Tree, inlinedSym: Symbol)(using Context): tpd.Tree = stat match
      case stat: ValDef =>
        inlinedValDef(stat, inlinedSym)
      case stat: DefDef =>
        inlinedDefDef(stat, inlinedSym)
      case stat @ TypeDef(_, _: Template) => EmptyTree
        /* inlinedClassDef(stat, inlinedSym.asClass) */ // Inner classes are not allowed for now.
      case stat: TypeDef =>
        inlinedTypeDef(stat, inlinedSym)

    private def inlinedSym(sym: Symbol, overriddenDecls: Set[Symbol], withoutFlags: FlagSet = EmptyFlags)(using Context): Symbol =
      val newSym = if sym.isClass then inlinedClassSym(sym.asClass, withoutFlags) else inlinedMemberSym(sym, overriddenDecls, withoutFlags)
      ctx.inlineTraitState.registerInlinedSymbol(sym, newSym, ctx.owner.thisType.classSymbol)
      newSym

    private def inlinedClassSym(sym: ClassSymbol, withoutFlags: FlagSet = EmptyFlags)(using Context): ClassSymbol =
      sym.info match {
        case clsInfo: ClassInfo =>
          val typeParams: List[Type] = sym.primaryConstructor.info match {
            case poly: PolyType => poly.paramRefs
            case _ => Nil
          }
          // Extend inner class from inline trait to preserve typing
          val newParent = ctx.owner.thisType.select(sym).appliedTo(typeParams)
          val inlinedSym = newClassSymbol(
            ctx.owner,
            sym.name,
            (sym.flags | Synthetic) &~ withoutFlags,
            newCls => {
              val ClassInfo(prefix, _, parents, _, selfInfo) = inlinerTypeMap.mapClassInfo(clsInfo)
              ClassInfo(prefix, newCls, parents :+ newParent, Scopes.newScope, selfInfo) // TODO fix selfInfo (what to use?)
            },
            sym.privateWithin,
            spanCoord(parent.span)
          )
          // ctx.inlineTraitState.registerInlinedInnerClassSymbol(sym, inlinedSym, childThisType)
          ctx.inlineTraitState.registerInlinedSymbol(sym, inlinedSym, childThisType.classSymbol)
          inlinedSym.entered
        case _ =>
          report.error(s"Class symbol ${sym.show} does not have class info")
          sym
      }

    private def inlinedMemberSym(sym: Symbol, overriddenDecls: Set[Symbol], withoutFlags: FlagSet = EmptyFlags)(using Context): Symbol =
      var name = sym.name
      var flags = sym.flags | Synthetic
      if sym.isTermParamAccessor then flags &~= ParamAccessor
      if sym.is(Local) || (overriddenDecls.contains(sym)) then
        name = paramAccessorsMapper.registerNewName(sym)
        flags |= (Private | Local)
        flags &~= Override          // private override is illegal; if the inlined method was already override then we might make it private override by accident.
      else
        flags |= Override
      sym.copy(
        owner = ctx.owner,
        name = name,
        flags = flags &~ withoutFlags,
        info = inlinerTypeMap(sym.info),
        coord = spanCoord(parent.span)).entered

    private def inlinedValDef(vdef: ValDef, inlinedSym: Symbol)(using Context): ValDef =
      val rhs =
        paramAccessorsMapper
          .getParamAccessorRhs(vdef.symbol.owner, vdef.symbol.name)
          .getOrElse(inlinedRhs(vdef, inlinedSym))
      
      val rhs1 = rhs.changeNonLocalOwners(inlinedSym) // if rhs.symbol.exists then rhs.changeOwner(rhs.symbol.owner, inlinedSym) else rhs

      tpd.ValDef(inlinedSym.asTerm, rhs1).withSpan(parent.span)

    private def inlinedDefDef(ddef: DefDef, inlinedSym: Symbol)(using Context): DefDef =
      val rhsFun: List[List[Tree]] => Tree =
        if ddef.symbol.isSetter then
          _ => unitLiteral
        else
          paramss =>
            val oldParamSyms = ddef.paramss.flatten.map(_.symbol)
            val newParamSyms = paramss.flatten.map(_.symbol)
            val ddef1 = cpy.DefDef(ddef)(rhs = ddef.rhs.subst(oldParamSyms, newParamSyms))
            inlinedRhs(ddef1, inlinedSym)
      tpd.DefDef(inlinedSym.asTerm, rhsFun).withSpan(parent.span)

    /*
    private def inlinedPrimaryConstructorDefDef(ddef: DefDef)(using Context): DefDef =
      // TODO check if symbol must be copied
      val inlinedSym = inlinedMemberSym(ddef.symbol, withoutFlags = Override)
      val constr = inlinedDefDef(ddef, inlinedSym)
      cpy.DefDef(constr)(tpt = TypeTree(defn.UnitType), rhs = EmptyTree)
    */
    /*
    private def inlinedClassDef(clsDef: TypeDef, inlinedCls: ClassSymbol)(using Context): Tree =
      val TypeDef(_, tmpl: Template) = clsDef: @unchecked
      val (constr, body) = inContext(ctx.withOwner(inlinedCls)) {
        val inlinedConstr = inlinedPrimaryConstructorDefDef(tmpl.constr)
        val inlinedTmpl = tmpl.body.map {
          case stat: TypeDef if stat.symbol.isAllOf(PrivateLocal | Param) =>
            expandStat(stat, inlinedSym(stat.symbol, withoutFlags = Override))
          case stat =>
            expandStat(stat, inlinedSym(stat.symbol))
        }
        (inlinedConstr, inlinedTmpl)
      }
      val clsDef1 = tpd.ClassDefWithParents(inlinedCls, constr, tmpl.parents, body) // TODO add correct parent tree
      inlined(clsDef1)._2.withSpan(clsDef.span)
    */

    private def inlinedTypeDef(tdef: TypeDef, inlinedSym: Symbol)(using Context): TypeDef =
      if inlinedSym.isOpaqueAlias then
        val inlinedRhsType = inlinerTypeMap(tdef.rhs.tpe)
        inlinedSym.info = inlinedSym.opaqueToBounds(TypeAlias(inlinedRhsType), tdef.rhs, List())
        inlinedSym.typeRef.recomputeDenot()
        ctx.typeAssigner.assignType(untpd.TypeDef(inlinedSym.name.asTypeName, TypeTree(inlinedRhsType)), inlinedSym).withSpan(parent.span)
      else
        tpd.TypeDef(inlinedSym.asType).withSpan(parent.span)
      

    private def inlinedRhs(vddef: ValOrDefDef, inlinedSym: Symbol)(using Context): Tree =
      val rhs = vddef.rhs.changeOwner(vddef.symbol, inlinedSym)
      inlinedRhs(rhs)(using ctx.withOwner(inlinedSym))

    private def inlinedRhs(rhs: Tree)(using Context): Tree =
      if rhs.isEmpty then
        rhs
      else
        val symbolMap = mutable.Map[Symbol, Symbol]()
        // TODO make version of inlined that does not return bindings?
        val rhs1 = Inlined(tpd.ref(parentSym).withSpan(parent.span), Nil, inlined(rhs)._2.withSpan(parent.span).cloneIn(parentSym.source)).withSpan(parent.span) // TODO: This inlines also calls to inline defs that were made in the inline trait body, is that desirable? 
        
        // In case of nested inline trait inlines, because BodyAnnotation is out of date,
        // body inlined misses nested expansion, but we have the symbols for the items that should be there
        // Remove them so that they can be inlined prperly later.
        val ttmap = TreeTypeMap(treeMap = {
          case tree@TypeDef(name, tmpl: Template) if Inlines.needsInlining(tree) => 
            val newSym = tree.symbol.copy(coord = spanCoord(tree.span))               // Coord should correspond to original location because we will inline from there.
            newSym.info = ClassInfo(tree.symbol.owner.thisType, newSym.asClass, tree.symbol.asClass.parentTypes, Scopes.newScope)

            val newConstructorSymbol = tree.symbol.primaryConstructor.copy(owner = newSym)
            val rt = newSym.typeRef.appliedTo(newSym.typeParams.map(_.typeRef))
            def resultType(tpe: Type): Type = tpe match {
                case mt @ MethodType(paramNames) => mt.derivedLambdaType(paramNames, mt.paramInfos, rt)
                case pt : PolyType => pt.derivedLambdaType(pt.paramNames, pt.paramInfos, resultType(pt.resType))
            }
            newConstructorSymbol.info = resultType(newConstructorSymbol.info)
            newConstructorSymbol.info = PolyType.fromParams(newConstructorSymbol.owner.typeParams, newConstructorSymbol.info)

            symbolMap(tree.symbol.primaryConstructor) = newConstructorSymbol

            val childSyms = tree.symbol.info.decls
              .filter(sym => tmpl.body.exists(vddef => vddef.symbol == sym))
              .tapEach(sym => symbolMap(sym) = sym.copy(owner = newSym, coord=sym.coord))
              .map(symbolMap)

            childSyms.foreach(p => p.entered)
            newConstructorSymbol.entered

            val tmpl1 = tmpl.changeOwner(tree.symbol, newSym)

            val rhsFun: List[List[Tree]] => Tree =
              paramss =>
                val oldParamSyms = tmpl1.constr.paramss.flatten.map(_.symbol)
                val newParamSyms = paramss.flatten.map(_.symbol)
                tmpl1.constr.rhs.subst(oldParamSyms, newParamSyms)

            val ctor = tpd.DefDef(newConstructorSymbol.asTerm, rhsFun)
            symbolMap(tree.symbol) = newSym

            ctx.inlineTraitState.registerInlineOrigin(newSym, child, parentSym)

            tpd.ClassDefWithParents(newSym.asClass, ctor, tmpl1.parents, tmpl1.body)
          case tree => tree
        })

        val rhs2 = ttmap(rhs1)
        TreeTypeMap(substFrom = symbolMap.keys.toList,
                    substTo = symbolMap.values.toList,
                    oldOwners = symbolMap.keys.toList,
                    newOwners = symbolMap.values.toList)(rhs2)

    private class ParamAccessorsMapper:
      private val paramAccessorsTrees: mutable.Map[Symbol, Map[Name, Tree]] = mutable.Map.empty
      private val paramAccessorsNewNames: mutable.Map[(Symbol, Name), Name] = mutable.Map.empty

      def registerParamValuesOf(parent: Tree): Unit =
        def allArgs(tree: Tree, acc: Vector[List[Tree]]): List[List[Tree]] = tree match
          case Apply(fun, args) => allArgs(fun, acc :+ args)
          case TypeApply(fun, _) => allArgs(fun, acc)
          case _ => acc.toList
        def allParams(info: Type, acc: List[List[Name]]): List[List[Name]] = info match
          case mt: MethodType => allParams(mt.resultType, mt.paramNames :: acc)
          case pt: PolyType => allParams(pt.resultType, acc)
          case _ => acc
        val info =
          if parent.symbol.isClass then parent.symbol.primaryConstructor.info
          else parent.symbol.info
        val paramAccessors = allParams(info, Nil).flatten.zip(allArgs(parent, Vector.empty).flatten).toMap
        paramAccessorsTrees.put(symbolFromParent(parent), paramAccessors)

      def registerNewName(paramAccessorSym: Symbol): paramAccessorSym.ThisName =
        val oldName = paramAccessorSym.name
        val newName = oldName.expandedName(parentSym)
        paramAccessorsNewNames.put((paramAccessorSym.owner, oldName), newName)
        newName

      def getParamAccessorRhs(parent: Symbol, paramAccessorName: Name): Option[Tree] =
        paramAccessorsTrees.get(parent).flatMap(_.get(paramAccessorName))

      def getParamAccessorName(parent: Symbol, paramAccessorName: Name): Option[Name] =
        paramAccessorsNewNames.get(parent, paramAccessorName)
    end ParamAccessorsMapper
  end InlineParentTrait

  class InlineTraitState:
    // Map representing all symbols we have inlined from inline traits,
    // from the symbol in the parent trait, and the symbol of the child class-like
    // to the inlined symbol in that child class-like.
    // E.g. inline trait A {def foo#1000}; trait B extends A {def foo#2000 // created by inlining}
    // The map has (foo#1000, trait B) => foo#2000
    val inlinedTraitSymbols = mutable.HashMap[(Symbol, Symbol), Symbol]()

    // For a class symbol created during inlining of an inline trait,
    // the chain of inlined traits which produced it. We don't actually care about the order. 
    val inlineOrigins = mutable.HashMap[Symbol, Set[Symbol]]().withDefaultValue(Set.empty)

    // Record that we just inlined oldSym into childClasslike which created
    // childClassLike.newSym
    def registerInlinedSymbol(oldSym: Symbol, newSym: Symbol, childClasslike: Symbol) =
      inlinedTraitSymbols((oldSym, childClasslike)) = newSym
    
    // Map (e.g.) B.foo#1000 into foo#2000 
    def lookupInlinedSymbol(oldSym: Symbol, childClasslike: Symbol) =
      inlinedTraitSymbols((oldSym, childClasslike))
    
    // Check if oldSym has been inlined into childClasslike
    def inlinedSymbolIsRegistered(oldSym: Symbol, childClasslike: Symbol) =
      inlinedTraitSymbols.contains((oldSym, childClasslike))

    def registerInlineOrigin(newSym: Symbol, owner: Symbol, parentSym: Symbol): Unit =
      inlineOrigins(newSym) = inlineOrigins(owner) + parentSym
  end InlineTraitState

end Inlines
