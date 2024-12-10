package dotty.tools
package dotc
package transform

import dotty.tools.dotc.ast.{Trees, tpd, untpd, desugar}
import scala.collection.mutable
import core.*
import dotty.tools.dotc.typer.Checking
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.typer.VarianceChecker
import typer.ErrorReporting.errorTree
import Types.*, Contexts.*, Names.*, Flags.*, DenotTransformers.*, Phases.*
import SymDenotations.*, StdNames.*, Annotations.*, Trees.*, Scopes.*
import Decorators.*
import Symbols.*, NameOps.*
import ContextFunctionResults.annotateContextResults
import config.Printers.typr
import config.Feature
import util.SrcPos
import reporting.*
import NameKinds.WildcardParamName
import cc.*
import dotty.tools.dotc.transform.MacroAnnotations.hasMacroAnnotation

object PostTyper {
  val name: String = "posttyper"
  val description: String = "additional checks and cleanups after type checking"
}

/** A macro transform that runs immediately after typer and that performs the following functions:
 *
 *  (1) Add super accessors (@see SuperAccessors)
 *
 *  (2) Convert parameter fields that have the same name as a corresponding
 *      public parameter field in a superclass to a forwarder to the superclass
 *      field (corresponding = super class field is initialized with subclass field)
 *      @see forwardParamAccessors.
 *
 *  (3) Add synthetic members (@see SyntheticMembers)
 *
 *  (4) Check that `New` nodes can be instantiated, and that annotations are valid
 *
 *  (5) Convert all trees representing types to TypeTrees.
 *
 *  (6) Check the bounds of AppliedTypeTrees
 *
 *  (7) Insert `.package` for selections of package object members
 *
 *  (8) Replaces self references by name with `this`
 *
 *  (9) Adds SourceFile annotations to all top-level classes and objects
 *
 *  (10) Adds Child annotations to all sealed classes
 *
 *  (11) Minimizes `call` fields of `Inlined` nodes to just point to the toplevel
 *       class from which code was inlined.
 *
 *  The reason for making this a macro transform is that some functions (in particular
 *  super and protected accessors and instantiation checks) are naturally top-down and
 *  don't lend themselves to the bottom-up approach of a mini phase. The other two functions
 *  (forwarding param accessors and synthetic methods) only apply to templates and fit
 *  mini-phase or subfunction of a macro phase equally well. But taken by themselves
 *  they do not warrant their own group of miniphases before pickling.
 */
class PostTyper extends MacroTransform with InfoTransformer { thisPhase =>
  import tpd.*

  override def phaseName: String = PostTyper.name

  override def description: String = PostTyper.description

  override def checkPostCondition(tree: tpd.Tree)(using Context): Unit = tree match {
    case tree: ValOrDefDef =>
      assert(!tree.symbol.signature.isUnderDefined)
    case _ =>
  }

  override def changesMembers: Boolean = true // the phase adds super accessors and synthetic members

  /**
   * Serializable and AbstractFunction1 are added for companion objects of case classes in scala2-library
   */
  override def changesParents: Boolean =
    if !initContextCalled then
      throw new Exception("Calling changesParents before initContext, should call initContext first")
    compilingScala2StdLib

  override def transformPhase(using Context): Phase = thisPhase.next

  def newTransformer(using Context): Transformer =
    new PostTyperTransformer

  /**
   * Used to check that `changesParents` is called after `initContext`.
   *
   * This contract is easy to break and results in subtle bugs.
   */
  private var initContextCalled = false

  private var compilingScala2StdLib = false
  override def initContext(ctx: FreshContext): Unit =
    initContextCalled = true
    compilingScala2StdLib = ctx.settings.YcompileScala2Library.value(using ctx)

  val superAcc: SuperAccessors = new SuperAccessors(thisPhase)
  val synthMbr: SyntheticMembers = new SyntheticMembers(thisPhase)
  val beanProps: BeanProperties = new BeanProperties(thisPhase)

  private def newPart(tree: Tree): Option[New] = methPart(tree) match {
    case Select(nu: New, _) => Some(nu)
    case _ => None
  }

  private def checkValidJavaAnnotation(annot: Tree)(using Context): Unit = {
    // TODO fill in
  }

  class PostTyperTransformer extends Transformer {

    private var inJavaAnnot: Boolean = false

    private var noCheckNews: Set[New] = Set()

    def withNoCheckNews[T](ts: List[New])(op: => T): T = {
      val saved = noCheckNews
      noCheckNews ++= ts
      try op finally noCheckNews = saved
    }

    def isCheckable(t: New): Boolean = !inJavaAnnot && !noCheckNews.contains(t)

    /** Mark parameter accessors that are aliases of like-named parameters
     *  in their superclass with SuperParamAlias.
     *  This info is used in phase ParamForwarding
     */
    private def forwardParamAccessors(impl: Template)(using Context): Unit = impl.parents match
      case superCall @ Apply(fn, superArgs) :: _
      if superArgs.nonEmpty && fn.symbol.isPrimaryConstructor =>
        fn.tpe.widen match
          case MethodType(superParamNames) =>
            for case stat: ValDef <- impl.body do
              val sym = stat.symbol
              if sym.isAllOf(PrivateParamAccessor, butNot = Mutable)
                 && !sym.info.isInstanceOf[ExprType] // val-parameters cannot be call-by name, so no need to try to forward to them
              then
                val idx = superArgs.indexWhere(_.symbol == sym)
                if idx >= 0 && superParamNames(idx) == stat.name then
                  // Supercall to like-named parameter.
                  // Having it have the same name is needed to maintain correctness in presence of subclassing
                  // if you would use parent param-name `a` to implement param-field `b`
                  // overriding field `b` will actually override field `a`, that is wrong!
                  typr.println(i"super alias: ${sym.showLocated}")
                  sym.setFlagFrom(thisPhase, SuperParamAlias)
          case _ =>
      case _ =>

    private def transformAnnot(annot: Tree)(using Context): Tree = {
      val saved = inJavaAnnot
      inJavaAnnot = annot.symbol.is(JavaDefined)
      if (inJavaAnnot) checkValidJavaAnnotation(annot)
      try transform(annot)
      finally inJavaAnnot = saved
    }

    private def transformAnnot(annot: Annotation)(using Context): Annotation =
      annot.derivedAnnotation(transformAnnot(annot.tree))

    private def processMemberDef(tree: Tree)(using Context): tree.type = {
      val sym = tree.symbol
      Checking.checkValidOperator(sym)
      sym.transformAnnotations(transformAnnot)
      sym.defTree = tree
      tree
    }

    private def processValOrDefDef(tree: Tree)(using Context): tree.type =
      val sym = tree.symbol
      tree match
        case tree: ValOrDefDef if !sym.is(Synthetic) =>
          checkInferredWellFormed(tree.tpt)
          if sym.is(Method) then
            if sym.isSetter then
              sym.keepAnnotationsCarrying(thisPhase, Set(defn.SetterMetaAnnot))
            if sym.isOneOf(GivenOrImplicit) then
              val cls = sym.info.finalResultType.classSymbol
              if cls.isOneOf(GivenOrImplicit) then
                sym.updateAnnotationsAfter(thisPhase,
                  atPhase(thisPhase)(cls.annotationsCarrying(Set(defn.CompanionMethodMetaAnnot)))
                    ++ sym.annotations)
          else
            if sym.is(Param) then
              sym.keepAnnotationsCarrying(thisPhase, Set(defn.ParamMetaAnnot), orNoneOf = defn.NonBeanMetaAnnots)
            else if sym.is(ParamAccessor) then
              // @publicInBinary is not a meta-annotation and therefore not kept by `keepAnnotationsCarrying`
              val publicInBinaryAnnotOpt = sym.getAnnotation(defn.PublicInBinaryAnnot)
              sym.keepAnnotationsCarrying(thisPhase, Set(defn.GetterMetaAnnot, defn.FieldMetaAnnot))
              for publicInBinaryAnnot <- publicInBinaryAnnotOpt do sym.addAnnotation(publicInBinaryAnnot)
            else
              sym.keepAnnotationsCarrying(thisPhase, Set(defn.GetterMetaAnnot, defn.FieldMetaAnnot), orNoneOf = defn.NonBeanMetaAnnots)
          if sym.isScala2Macro && !ctx.settings.XignoreScala2Macros.value then
            if !sym.owner.unforcedDecls.exists(p => !p.isScala2Macro && p.name == sym.name && p.signature == sym.signature)
               // Allow scala.reflect.materializeClassTag to be able to compile scala/reflect/package.scala
               // This should be removed on Scala 3.x
               && sym.owner != defn.ReflectPackageClass
            then
              report.error("No Scala 3 implementation found for this Scala 2 macro.", tree.srcPos)
        case _ =>
      processMemberDef(tree)

    private def checkInferredWellFormed(tree: Tree)(using Context): Unit = tree match
      case tree: TypeTree
      if tree.span.isZeroExtent
          // don't check TypeTrees with non-zero extent;
          // these are derived from explicit types
         && !ctx.reporter.errorsReported
          // don't check if errors were already reported; this avoids follow-on errors
          // for inferred types if explicit types are already ill-formed
        => Checking.checkAppliedTypesIn(tree)
      case _ =>


    private def transformSelect(tree: Select, targs: List[Tree])(using Context): Tree = {
      val qual = tree.qualifier
      qual.symbol.moduleClass.denot match {
        case pkg: PackageClassDenotation =>
          val pobj = pkg.packageObjFor(tree.symbol)
          if (pobj.exists)
            return transformSelect(cpy.Select(tree)(qual.select(pobj).withSpan(qual.span), tree.name), targs)
        case _ =>
      }
      val tree1 = super.transform(tree)
      constToLiteral(tree1) match {
        case _: Literal => tree1
        case _ => superAcc.transformSelect(tree1, targs)
      }
    }

    private def normalizeTypeArgs(tree: TypeApply)(using Context): TypeApply = tree.tpe match {
      case pt: PolyType => // wait for more arguments coming
        tree
      case _ =>
        def decompose(tree: TypeApply): (Tree, List[Tree]) = tree.fun match {
          case fun: TypeApply =>
            val (tycon, args) = decompose(fun)
            (tycon, args ++ tree.args)
          case _ =>
            (tree.fun, tree.args)
        }
        def reorderArgs(pnames: List[Name], namedArgs: List[NamedArg], otherArgs: List[Tree]): List[Tree] = pnames match {
          case pname :: pnames1 =>
            namedArgs.partition(_.name == pname) match {
              case (NamedArg(_, arg) :: _, namedArgs1) =>
                arg :: reorderArgs(pnames1, namedArgs1, otherArgs)
              case _ =>
                val otherArg :: otherArgs1 = otherArgs: @unchecked
                otherArg :: reorderArgs(pnames1, namedArgs, otherArgs1)
            }
          case nil =>
            assert(namedArgs.isEmpty && otherArgs.isEmpty)
            Nil
        }
        val (tycon, args) = decompose(tree)
        tycon.tpe.widen match {
          case tp: PolyType if args.exists(isNamedArg) =>
            val (namedArgs, otherArgs) = args.partition(isNamedArg)
            val args1 = reorderArgs(tp.paramNames, namedArgs.asInstanceOf[List[NamedArg]], otherArgs)
            TypeApply(tycon, args1).withSpan(tree.span).withType(tree.tpe)
          case _ =>
            tree
        }
    }

    private object dropInlines extends TreeMap {
      override def transform(tree: Tree)(using Context): Tree = tree match {
        case tree @ Inlined(call, _, expansion) =>
          val newExpansion = PruneErasedDefs.trivialErasedTree(tree)
          cpy.Inlined(tree)(call, Nil, newExpansion)
        case _ => super.transform(tree)
      }
    }

    def checkUsableAsValue(tree: Tree)(using Context): Tree =
      def unusable(msg: Symbol => Message) =
        errorTree(tree, msg(tree.symbol))
      if tree.symbol.is(ConstructorProxy) then
        unusable(ConstructorProxyNotValue(_))
      else if tree.symbol.isContextBoundCompanion then
        unusable(ContextBoundCompanionNotValue(_))
      else
        tree

    def checkStableSelection(tree: Tree)(using Context): Unit =
      def check(qual: Tree) =
        if !qual.tpe.isStable then
          report.error(em"Parameter untupling cannot be used for call-by-name parameters", tree.srcPos)
      appliedCore(closureBody(tree)) match
        case Select(qual, _) => check(qual)
        // simple select _n                    Select(qual, _n)
        // generic select .apply[T](n)         Apply(TypeApply(Select(qual, _), _), _)
        // context closure x ?=> f(using x)    Block(List(DefDef($anonfun, _, _, Apply(Select(Select(qual, _n), _), _)))

    def checkNotPackage(tree: Tree)(using Context): Tree =
      if !tree.symbol.is(Package) then tree
      else errorTree(tree, em"${tree.symbol} cannot be used as a type")

    /** Make result types of ValDefs and DefDefs that override some other definitions
     *  declared types rather than InferredTypes. This is necessary since we otherwise
     *  clean retains annotations from such types. But for an overriding symbol the
     *  retains annotations come from the explicitly declared parent types, so should
     *  be kept.
     */
    private def makeOverrideTypeDeclared(symbol: Symbol, tpt: Tree)(using Context): Tree =
      tpt match
        case tpt: InferredTypeTree
        if symbol.allOverriddenSymbols.hasNext =>
          TypeTree(tpt.tpe, inferred = false).withSpan(tpt.span).withAttachmentsFrom(tpt)
        case _ =>
          tpt

    override def transform(tree: Tree)(using Context): Tree =
      try tree match {
        // TODO move CaseDef case lower: keep most probable trees first for performance
        case CaseDef(pat, _, _) =>
          val gadtCtx =
           pat.removeAttachment(typer.Typer.InferredGadtConstraints) match
             case Some(gadt) => ctx.fresh.setGadtState(GadtState(gadt))
             case None =>
               ctx
          super.transform(tree)(using gadtCtx)
        case tree: Ident =>
          if tree.isType then
            checkNotPackage(tree)
          else
            registerNeedsInlining(tree)
            val tree1 = checkUsableAsValue(tree)
            tree1.tpe match {
              case tpe: ThisType => This(tpe.cls).withSpan(tree.span)
              case _ => tree1
            }
        case tree @ Select(qual, name) =>
          registerNeedsInlining(tree)
          if name.isTypeName then
            Checking.checkRealizable(qual.tpe, qual.srcPos)
            withMode(Mode.Type)(super.transform(checkNotPackage(tree)))
          else
            checkUsableAsValue(tree) match
              case tree1: Select => transformSelect(tree1, Nil)
              case tree1 => tree1
        case tree: Apply =>
          val methType = tree.fun.tpe.widen.asInstanceOf[MethodType]
          val app =
            if (methType.hasErasedParams)
              tpd.cpy.Apply(tree)(
                tree.fun,
                tree.args.zip(methType.erasedParams).map((arg, isErased) =>
                  if !isErased then arg
                  else
                    if methType.isResultDependent then
                      Checking.checkRealizable(arg.tpe, arg.srcPos, "erased argument")
                    if (methType.isImplicitMethod && arg.span.isSynthetic)
                      arg match
                        case _: RefTree | _: Apply | _: TypeApply if arg.symbol.is(Erased) =>
                          dropInlines.transform(arg)
                        case _ =>
                          PruneErasedDefs.trivialErasedTree(arg)
                    else dropInlines.transform(arg)))
            else
              tree
          def app1 =
            // reverse order of transforming args and fun. This way, we get a chance to see other
            // well-formedness errors before reporting errors in possible inferred type args of fun.
            val args1 = transform(app.args)
            cpy.Apply(app)(transform(app.fun), args1)
          methPart(app) match
            case Select(nu: New, nme.CONSTRUCTOR) if isCheckable(nu) =>
              // need to check instantiability here, because the type of the New itself
              // might be a type constructor.
              def checkClassType(tpe: Type, stablePrefixReq: Boolean) =
                ctx.typer.checkClassType(tpe, tree.srcPos,
                    traitReq = false, stablePrefixReq = stablePrefixReq,
                    refinementOK = Feature.enabled(Feature.modularity))
              checkClassType(tree.tpe, true)
              if !nu.tpe.isLambdaSub then
                // Check the constructor type as well; it could be an illegal singleton type
                // which would not be reflected as `tree.tpe`
                checkClassType(nu.tpe, false)
              Checking.checkInstantiable(tree.tpe, nu.tpe, nu.srcPos)
              withNoCheckNews(nu :: Nil)(app1)
            case _ =>
              app1
        case UnApply(fun, implicits, patterns) =>
          // Reverse transform order for the same reason as in `app1` above.
          val patterns1 = transform(patterns)
          cpy.UnApply(tree)(transform(fun), transform(implicits), patterns1)
        case tree: TypeApply =>
          if tree.symbol == defn.QuotedTypeModule_of then
            ctx.compilationUnit.needsStaging = true
          registerNeedsInlining(tree)
          val tree1 @ TypeApply(fn, args) = normalizeTypeArgs(tree)
          for arg <- args do
            checkInferredWellFormed(arg)
          if (fn.symbol != defn.ChildAnnot.primaryConstructor)
            // Make an exception for ChildAnnot, which should really have AnyKind bounds
            Checking.checkBounds(args, fn.tpe.widen.asInstanceOf[PolyType])
          fn match {
            case sel: Select =>
              val args1 = transform(args)
              val sel1 = transformSelect(sel, args1)
              cpy.TypeApply(tree1)(sel1, args1)
            case _ =>
              super.transform(tree1)
          }
        case tree @ Inlined(call, bindings, expansion) if !tree.inlinedFromOuterScope =>
          val pos = call.sourcePos
          CrossVersionChecks.checkRef(call.symbol, pos)
          withMode(Mode.NoInline)(transform(call))
          val callTrace = Inlines.inlineCallTrace(call.symbol, pos)(using ctx.withSource(pos.source))
          cpy.Inlined(tree)(callTrace, transformSub(bindings), transform(expansion)(using inlineContext(tree)))
        case templ: Template =>
          Checking.checkPolyFunctionExtension(templ)
          withNoCheckNews(templ.parents.flatMap(newPart)) {
            forwardParamAccessors(templ)
            synthMbr.addSyntheticMembers(
              beanProps.addBeanMethods(
                superAcc.wrapTemplate(templ)(
                  super.transform(_).asInstanceOf[Template]))
            )
          }
        case tree: ValDef =>
          annotateExperimentalCompanion(tree.symbol)
          registerIfHasMacroAnnotations(tree)
          checkErasedDef(tree)
          Checking.checkPolyFunctionType(tree.tpt)
          val tree1 = cpy.ValDef(tree)(tpt = makeOverrideTypeDeclared(tree.symbol, tree.tpt), rhs = normalizeErasedRhs(tree.rhs, tree.symbol))
          if tree1.removeAttachment(desugar.UntupledParam).isDefined then
            checkStableSelection(tree.rhs)
          processValOrDefDef(super.transform(tree1))
        case tree: DefDef =>
          registerIfHasMacroAnnotations(tree)
          checkErasedDef(tree)
          Checking.checkPolyFunctionType(tree.tpt)
          annotateContextResults(tree)
          val tree1 = cpy.DefDef(tree)(tpt = makeOverrideTypeDeclared(tree.symbol, tree.tpt), rhs = normalizeErasedRhs(tree.rhs, tree.symbol))
          processValOrDefDef(superAcc.wrapDefDef(tree1)(super.transform(tree1).asInstanceOf[DefDef]))
        case tree: TypeDef =>
          registerIfHasMacroAnnotations(tree)
          val sym = tree.symbol
          if (sym.isClass)
            VarianceChecker.check(tree)
            annotateExperimentalCompanion(sym)
            checkMacroAnnotation(sym)
            if sym.isOneOf(GivenOrImplicit) then
              sym.keepAnnotationsCarrying(thisPhase, Set(defn.CompanionClassMetaAnnot), orNoneOf = defn.MetaAnnots)
            tree.rhs match
              case impl: Template =>
                for parent <- impl.parents do
                  Checking.checkTraitInheritance(parent.tpe.classSymbol, sym.asClass, parent.srcPos)
                  // Constructor parameters are in scope when typing a parent.
                  // While they can safely appear in a parent tree, to preserve
                  // soundness we need to ensure they don't appear in a parent
                  // type (#16270). We can strip any refinement of a parent type since
                  // these refinements are split off from the parent type constructor
                  // application `parent` in Namer and don't show up as parent types
                  // of the class.
                  val illegalRefs = parent.tpe.dealias.stripRefinement.namedPartsWith:
                      p => p.symbol.is(ParamAccessor) && (p.symbol.owner eq sym)
                  if illegalRefs.nonEmpty then
                    report.error(
                      em"The type of a class parent cannot refer to constructor parameters, but ${parent.tpe} refers to ${illegalRefs.map(_.name.show).mkString(",")}", parent.srcPos)
            if sym.owner.is(Package) then
              // Add SourceFile annotation to top-level classes
              // TODO remove this annotation once the reference compiler uses the TASTy source file attribute.
              if ctx.compilationUnit.source.exists && sym != defn.SourceFileAnnot then
                val reference = ctx.settings.sourceroot.value
                val relativePath = util.SourceFile.relativePath(ctx.compilationUnit.source, reference)
                sym.addAnnotation(Annotation(defn.SourceFileAnnot, Literal(Constants.Constant(relativePath)), tree.span))
          else
            if !sym.is(Param) && !sym.owner.isOneOf(AbstractOrTrait) then
              Checking.checkGoodBounds(tree.symbol)
            // Delete all context bound companions of this TypeDef
            if sym.owner.isClass && sym.hasAnnotation(defn.WitnessNamesAnnot) then
              val decls = sym.owner.info.decls
              for cbCompanion <- decls.lookupAll(sym.name.toTermName) do
                if cbCompanion.isContextBoundCompanion then
                  decls.openForMutations.unlink(cbCompanion)
            (tree.rhs, sym.info) match
              case (rhs: LambdaTypeTree, bounds: TypeBounds) =>
                VarianceChecker.checkLambda(rhs, bounds)
                if sym.isOpaqueAlias then
                  VarianceChecker.checkLambda(rhs, TypeBounds.upper(sym.opaqueAlias))
              case _ =>
          processMemberDef(super.transform(scala2LibPatch(tree)))
        case tree: Bind =>
          if tree.symbol.isType && !tree.symbol.name.is(WildcardParamName) then
            Checking.checkGoodBounds(tree.symbol)
          super.transform(tree)
        case tree: New if isCheckable(tree) =>
          Checking.checkInstantiable(tree.tpe, tree.tpe, tree.srcPos)
          super.transform(tree)
        case tree: Closure if !tree.tpt.isEmpty =>
          Checking.checkRealizable(tree.tpt.tpe, tree.srcPos, "SAM type")
          super.transform(tree)
        case tree @ Annotated(annotated, annot) =>
          cpy.Annotated(tree)(transform(annotated), transformAnnot(annot))
        case tree: AppliedTypeTree =>
          if (tree.tpt.symbol == defn.andType)
            Checking.checkNonCyclicInherited(tree.tpe, tree.args.tpes, EmptyScope, tree.srcPos)
              // Ideally, this should be done by Typer, but we run into cyclic references
              // when trying to typecheck self types which are intersections.
          else if (tree.tpt.symbol == defn.orType)
            () // nothing to do
          else
            Checking.checkAppliedType(tree)
          super.transform(tree)
        case SingletonTypeTree(ref) =>
          Checking.checkRealizable(ref.tpe, ref.srcPos)
          super.transform(tree)
        case tree: TypeBoundsTree =>
          val TypeBoundsTree(lo, hi, alias) = tree
          if !alias.isEmpty then
            val bounds = TypeBounds(lo.tpe, hi.tpe)
            if !bounds.contains(alias.tpe) then
              report.error(em"type ${alias.tpe} outside bounds $bounds", tree.srcPos)
          super.transform(tree)
        case tree: TypeTree =>
          val tpe = if tree.isInferred then CleanupRetains()(tree.tpe) else tree.tpe
          tree.withType:
            tpe match
              case AnnotatedType(parent, annot) =>
                AnnotatedType(parent, transformAnnot(annot)) // TODO: Also map annotations embedded in type?
              case _ => tpe
        case Typed(Ident(nme.WILDCARD), _) =>
          withMode(Mode.Pattern)(super.transform(tree))
            // The added mode signals that bounds in a pattern need not
            // conform to selector bounds. I.e. assume
            //     type Tree[T >: Null <: Type]
            // One is still allowed to write
            //     case x: Tree[?]
            // (which translates to)
            //     case x: (_: Tree[?])
        case m @ MatchTypeTree(bounds, selector, cases) =>
          // Analog to the case above for match types
          def transformIgnoringBoundsCheck(x: CaseDef): CaseDef =
            withMode(Mode.Pattern)(super.transform(x)).asInstanceOf[CaseDef]
          cpy.MatchTypeTree(tree)(
            super.transform(bounds),
            super.transform(selector),
            cases.mapConserve(transformIgnoringBoundsCheck)
          )
        case Block(_, Closure(_, _, tpt)) if ExpandSAMs.needsWrapperClass(tpt.tpe) =>
          superAcc.withInvalidCurrentClass(super.transform(tree))
        case tree: RefinedTypeTree =>
          Checking.checkPolyFunctionType(tree)
          super.transform(tree)
        case _: Quote | _: QuotePattern =>
          ctx.compilationUnit.needsStaging = true
          super.transform(tree)
        case tree =>
          super.transform(tree)
      }
      catch {
        case ex : AssertionError =>
          println(i"error while transforming $tree")
          throw ex
      }

    override def transformStats[T](trees: List[Tree], exprOwner: Symbol, wrapResult: List[Tree] => Context ?=> T)(using Context): T =
      Checking.checkAndAdaptExperimentalImports(trees)
      super.transformStats(trees, exprOwner, wrapResult)

    /** Transforms the rhs tree into a its default tree if it is in an `erased` val/def.
     *  Performed to shrink the tree that is known to be erased later.
     */
    private def normalizeErasedRhs(rhs: Tree, sym: Symbol)(using Context) =
      if (sym.isEffectivelyErased) dropInlines.transform(rhs) else rhs

    private def registerNeedsInlining(tree: Tree)(using Context): Unit =
      if tree.symbol.is(Inline) && !Inlines.inInlineMethod && !ctx.mode.is(Mode.NoInline) then
        ctx.compilationUnit.needsInlining = true

    /** Check if the definition has macro annotation and sets `compilationUnit.hasMacroAnnotations` if needed. */
    private def registerIfHasMacroAnnotations(tree: DefTree)(using Context) =
      if !Inlines.inInlineMethod && tree.symbol.hasMacroAnnotation then
        ctx.compilationUnit.hasMacroAnnotations = true

    /** Check macro annotations implementations  */
    private def checkMacroAnnotation(sym: Symbol)(using Context) =
      if sym.derivesFrom(defn.MacroAnnotationClass) && !sym.isStatic then
        report.error("classes that extend MacroAnnotation must not be inner/local classes", sym.srcPos)

    private def checkErasedDef(tree: ValOrDefDef)(using Context): Unit =
      def checkOnlyErasedParams(): Unit = tree match
        case tree: DefDef =>
          for params <- tree.paramss; param <- params if !param.symbol.isType && !param.symbol.is(Erased) do
            report.error("erased definition can only have erased parameters", param.srcPos)
        case _ =>

      if tree.symbol.is(Erased, butNot = Macro) then
        checkOnlyErasedParams()
        val tpe = tree.rhs.tpe
        if tpe.derivesFrom(defn.NothingClass) then
          report.error("`erased` definition cannot be implemented with en expression of type Nothing", tree.srcPos)
        else if tpe.derivesFrom(defn.NullClass) then
          report.error("`erased` definition cannot be implemented with en expression of type Null", tree.srcPos)

    private def annotateExperimentalCompanion(sym: Symbol)(using Context): Unit =
      if sym.is(Module) then
        ExperimentalAnnotation.copy(sym.companionClass).foreach(sym.addAnnotation)

    // It needs to run at the phase of the postTyper --- otherwise, the test of the symbols will use
    // the transformed denotation with added `Serializable` and `AbstractFunction1`.
    private def scala2LibPatch(tree: TypeDef)(using Context) = atPhase(thisPhase):
      val sym = tree.symbol
      if compilingScala2StdLib && sym.is(ModuleClass) then
        // Add Serializable to companion objects of serializable classes,
        // and add AbstractFunction1 to companion objects of case classes with 1 parameter.
        tree.rhs match
          case impl: Template =>
            var parents1 = impl.parents
            val companionClass = sym.companionClass
            if !sym.derivesFrom(defn.SerializableClass) && companionClass.derivesFrom(defn.SerializableClass) then
              parents1 = parents1 :+ TypeTree(defn.SerializableType)
            argTypeOfCaseClassThatNeedsAbstractFunction1(sym) match
              case Some(args) if parents1.head.symbol.owner == defn.ObjectClass =>
                parents1 = New(defn.AbstractFunctionClass(1).typeRef).select(nme.CONSTRUCTOR).appliedToTypes(args).ensureApplied :: parents1.tail
              case _ =>
            val impl1 = cpy.Template(impl)(parents = parents1)
            cpy.TypeDef(tree)(rhs = impl1)
      else tree
  }

  protected override def infoMayChange(sym: Symbol)(using Context): Boolean =
    compilingScala2StdLib && sym.isAllOf(ModuleClass, butNot = Package)

  def transformInfo(tp: Type, sym: Symbol)(using Context): Type = tp match
    case info: ClassInfo =>
      var parents1 = info.parents
      val companionClass = sym.companionClass
      if !sym.derivesFrom(defn.SerializableClass) && companionClass.derivesFrom(defn.SerializableClass) then
        parents1 = parents1 :+ defn.SerializableType
      argTypeOfCaseClassThatNeedsAbstractFunction1(sym) match
        case Some(args) if parents1.head.typeSymbol == defn.ObjectClass =>
          parents1 = defn.AbstractFunctionClass(1).typeRef.appliedTo(args) :: parents1.tail
        case _ =>
      if parents1 ne info.parents then info.derivedClassInfo(declaredParents = parents1)
      else tp
    case _ => tp

  private def argTypeOfCaseClassThatNeedsAbstractFunction1(sym: Symbol)(using Context): Option[List[Type]] =
    val companionClass = sym.companionClass
    if companionClass.is(CaseClass)
      && !companionClass.primaryConstructor.is(Private)
      && !companionClass.primaryConstructor.info.isVarArgsMethod
    then
      sym.info.decl(nme.apply).info match
        case info: MethodType =>
          info.paramInfos match
            case arg :: Nil =>
              Some(arg :: info.resultType :: Nil)
            case args => None
        case _ => None
    else
      None
}
