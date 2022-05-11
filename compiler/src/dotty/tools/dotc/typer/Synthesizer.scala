package dotty.tools
package dotc
package typer

import core._
import util.Spans.Span
import Contexts._
import Types._, Flags._, Symbols._, Names._, StdNames._, Constants._
import TypeErasure.{erasure, hasStableErasure}
import Decorators._
import ProtoTypes._
import Inferencing.fullyDefinedType
import ast.untpd
import transform.SymUtils._
import transform.TypeUtils._
import transform.SyntheticMembers._
import util.Property
import ast.Trees.genericEmptyTree
import annotation.{tailrec, constructorOnly}
import ast.tpd._
import Synthesizer._

/** Synthesize terms for special classes */
class Synthesizer(typer: Typer)(using @constructorOnly c: Context):

  /** Handlers to synthesize implicits for special types */
  type SpecialHandler = (Type, Span) => Context ?=> TreeWithErrors
  private type SpecialHandlers = List[(ClassSymbol, SpecialHandler)]
  
  val synthesizedClassTag: SpecialHandler = (formal, span) =>
    formal.argInfos match
      case arg :: Nil =>
        fullyDefinedType(arg, "ClassTag argument", span) match
          case defn.ArrayOf(elemTp) =>
            val etag = typer.inferImplicitArg(defn.ClassTagClass.typeRef.appliedTo(elemTp), span)
            if etag.tpe.isError then EmptyTreeNoError else withNoErrors(etag.select(nme.wrap))
          case tp if hasStableErasure(tp) && !defn.isBottomClassAfterErasure(tp.typeSymbol) =>
            val sym = tp.typeSymbol
            val classTag = ref(defn.ClassTagModule)
            val tag =
              if defn.SpecialClassTagClasses.contains(sym) then
                classTag.select(sym.name.toTermName)
              else
                val clsOfType = escapeJavaArray(erasure(tp))
                classTag.select(nme.apply).appliedToType(tp).appliedTo(clsOf(clsOfType))
            withNoErrors(tag.withSpan(span))
          case tp => EmptyTreeNoError
      case _ => EmptyTreeNoError
  end synthesizedClassTag

  val synthesizedTypeTest: SpecialHandler =
    (formal, span) => formal.argInfos match {
      case arg1 :: arg2 :: Nil if !defn.isBottomClass(arg2.typeSymbol) =>
        val tp1 = fullyDefinedType(arg1, "TypeTest argument", span)
        val tp2 = fullyDefinedType(arg2, "TypeTest argument", span).normalized
        val sym2 = tp2.typeSymbol
        if tp1 <:< tp2 then
          // optimization when we know the typetest will always succeed
          withNoErrors(ref(defn.TypeTestModule_identity).appliedToType(tp2).withSpan(span))
        else if sym2 == defn.AnyValClass || sym2 == defn.AnyRefAlias || sym2 == defn.ObjectClass then
          EmptyTreeNoError
        else
          // Generate SAM: (s: <tp1>) => if s.isInstanceOf[<tp2>] then Some(s.asInstanceOf[s.type & <tp2>]) else None
          def body(args: List[Tree]): Tree = {
            val arg :: Nil = args
            val t = arg.tpe & tp2
            If(
              arg.isInstance(tp2),
              ref(defn.SomeClass.companionModule.termRef).select(nme.apply)
                .appliedToType(t)
                .appliedTo(arg.select(nme.asInstanceOf_).appliedToType(t)),
              ref(defn.NoneModule))
          }
          val tpe = MethodType(List(nme.s))(_ => List(tp1), mth => defn.OptionClass.typeRef.appliedTo(mth.newParamRef(0) & tp2))
          val meth = newAnonFun(ctx.owner, tpe, coord = span)
          val typeTestType = defn.TypeTestClass.typeRef.appliedTo(List(tp1, tp2))
          withNoErrors(Closure(meth, tss => body(tss.head).changeOwner(ctx.owner, meth), targetType = typeTestType).withSpan(span))
      case _ =>
        EmptyTreeNoError
    }
  end synthesizedTypeTest

  val synthesizedTupleFunction: SpecialHandler = (formal, span) =>
    formal match
      case AppliedType(_, funArgs @ fun :: tupled :: Nil) =>
        def functionTypeEqual(baseFun: Type, actualArgs: List[Type],
            actualRet: Type, expected: Type) =
          expected =:= defn.FunctionOf(actualArgs, actualRet,
            defn.isContextFunctionType(baseFun), defn.isErasedFunctionType(baseFun))
        val arity: Int =
          if defn.isErasedFunctionType(fun) || defn.isErasedFunctionType(fun) then -1 // TODO support?
          else if defn.isFunctionType(fun) then
            // TupledFunction[(...) => R, ?]
            fun.dropDependentRefinement.dealias.argInfos match
              case funArgs :+ funRet
              if functionTypeEqual(fun, defn.tupleType(funArgs) :: Nil, funRet, tupled) =>
                // TupledFunction[(...funArgs...) => funRet, ?]
                funArgs.size
              case _ => -1
          else if defn.isFunctionType(tupled) then
            // TupledFunction[?, (...) => R]
            tupled.dropDependentRefinement.dealias.argInfos match
              case tupledArgs :: funRet :: Nil =>
                defn.tupleTypes(tupledArgs.dealias) match
                  case Some(funArgs) if functionTypeEqual(tupled, funArgs, funRet, fun) =>
                    // TupledFunction[?, ((...funArgs...)) => funRet]
                    funArgs.size
                  case _ => -1
              case _ => -1
          else
            // TupledFunction[?, ?]
            -1
        if arity == -1 then
          EmptyTreeNoError
        else if arity <= Definitions.MaxImplementedFunctionArity then
          withNoErrors(ref(defn.RuntimeTupleFunctionsModule)
            .select(s"tupledFunction$arity".toTermName)
            .appliedToTypes(funArgs))
        else
          withNoErrors(ref(defn.RuntimeTupleFunctionsModule)
            .select("tupledFunctionXXL".toTermName)
            .appliedToTypes(funArgs))
      case _ =>
        EmptyTreeNoError
  end synthesizedTupleFunction

  /** If `formal` is of the form CanEqual[T, U], try to synthesize an
    *  `CanEqual.canEqualAny[T, U]` as solution.
    */
  val synthesizedCanEqual: SpecialHandler = (formal, span) =>

    /** Is there an `CanEqual[T, T]` instance, assuming -strictEquality? */
    def hasEq(tp: Type)(using Context): Boolean =
      val inst = typer.inferImplicitArg(defn.CanEqualClass.typeRef.appliedTo(tp, tp), span)
      !inst.isEmpty && !inst.tpe.isError

    /** Can we assume the canEqualAny instance for `tp1`, `tp2`?
      *  This is the case if assumedCanEqual(tp1, tp2), or
      *  one of `tp1`, `tp2` has a reflexive `CanEqual` instance.
      */
    def validEqAnyArgs(tp1: Type, tp2: Type)(using Context) =
      typer.assumedCanEqual(tp1, tp2)
       || withMode(Mode.StrictEquality) {
            !hasEq(tp1) && !hasEq(tp2)
          }

    /** Is an `CanEqual[cls1, cls2]` instance assumed for predefined classes `cls1`, cls2`? */
    def canComparePredefinedClasses(cls1: ClassSymbol, cls2: ClassSymbol): Boolean =

      def cmpWithBoxed(cls1: ClassSymbol, cls2: ClassSymbol) =
        cls2 == defn.NothingClass
        || cls2 == defn.boxedType(cls1.typeRef).symbol
        || cls1.isNumericValueClass && cls2.derivesFrom(defn.BoxedNumberClass)

      if cls1.isPrimitiveValueClass then
        if cls2.isPrimitiveValueClass then
          cls1 == cls2 || cls1.isNumericValueClass && cls2.isNumericValueClass
        else
          cmpWithBoxed(cls1, cls2)
      else if cls2.isPrimitiveValueClass then
        cmpWithBoxed(cls2, cls1)
      else if ctx.mode.is(Mode.SafeNulls) then
        // If explicit nulls is enabled, and unsafeNulls is not enabled,
        // we want to disallow comparison between Object and Null.
        // If we have to check whether a variable with a non-nullable type has null value
        // (for example, a NotNull java method returns null for some reasons),
        // we can still cast it to a nullable type then compare its value.
        //
        // Example:
        // val x: String = null.asInstanceOf[String]
        // if (x == null) {} // error: x is non-nullable
        // if (x.asInstanceOf[String|Null] == null) {} // ok
        cls1 == defn.NullClass && cls1 == cls2
      else if cls1 == defn.NullClass then
        cls1 == cls2 || cls2.derivesFrom(defn.ObjectClass)
      else if cls2 == defn.NullClass then
        cls1.derivesFrom(defn.ObjectClass)
      else
        cls1 == defn.NothingClass || cls2 == defn.NothingClass
    end canComparePredefinedClasses

    /** Some simulated `CanEqual` instances for predefined types. It's more efficient
      *  to do this directly instead of setting up a lot of `CanEqual` instances to
      *  interpret.
      */
    def canComparePredefined(tp1: Type, tp2: Type) =
      tp1.classSymbols.exists(cls1 =>
        tp2.classSymbols.exists(cls2 =>
          canComparePredefinedClasses(cls1, cls2)))

    formal.argTypes match
      case args @ (arg1 :: arg2 :: Nil) =>
        List(arg1, arg2).foreach(fullyDefinedType(_, "eq argument", span))
        if canComparePredefined(arg1, arg2)
            || !Implicits.strictEquality && explore(validEqAnyArgs(arg1, arg2))
        then withNoErrors(ref(defn.CanEqual_canEqualAny).appliedToTypes(args).withSpan(span))
        else EmptyTreeNoError
      case _ => EmptyTreeNoError
  end synthesizedCanEqual

  /** Creates a tree that will produce a ValueOf instance for the requested type.
   * An EmptyTreeNoError is returned if materialization fails.
   */
  val synthesizedValueOf: SpecialHandler = (formal, span) =>

    def success(t: Tree) =
      New(defn.ValueOfClass.typeRef.appliedTo(t.tpe), t :: Nil).withSpan(span)
    formal.argInfos match
      case arg :: Nil =>
        fullyDefinedType(arg, "ValueOf argument", span).normalized.dealias match
          case ConstantType(c: Constant) =>
            withNoErrors(success(Literal(c)))
          case tp: TypeRef if tp.isRef(defn.UnitClass) =>
            withNoErrors(success(Literal(Constant(()))))
          case n: TermRef =>
            withNoErrors(success(ref(n)))
          case tp =>
            EmptyTreeNoError
      case _ =>
        EmptyTreeNoError
  end synthesizedValueOf

  /** Create an anonymous class `new Object { type MirroredMonoType = ... }`
   *  and mark it with given attachment so that it is made into a mirror at PostTyper.
   */
  private def anonymousMirror(monoType: Type, attachment: Property.StickyKey[Unit], span: Span)(using Context) =
    if ctx.isAfterTyper then ctx.compilationUnit.needsMirrorSupport = true
    val monoTypeDef = untpd.TypeDef(tpnme.MirroredMonoType, untpd.TypeTree(monoType))
    val newImpl = untpd.Template(
      constr = untpd.emptyConstructor,
      parents = untpd.TypeTree(defn.ObjectType) :: Nil,
      derived = Nil,
      self = EmptyValDef,
      body = monoTypeDef :: Nil
    ).withAttachment(attachment, ())
    typer.typed(untpd.New(newImpl).withSpan(span))

  /** The mirror type
   *
   *     <parent> {
   *       MirroredMonoType = <monoType>
   *       MirroredType = <mirroredType>
   *       MirroredLabel = <label> }
   *     }
   */
  private def mirrorCore(parentClass: ClassSymbol, monoType: Type, mirroredType: Type, label: Name, formal: Type)(using Context) =
    formal & parentClass.typeRef
      .refinedWith(tpnme.MirroredMonoType, TypeAlias(monoType))
      .refinedWith(tpnme.MirroredType, TypeAlias(mirroredType))
      .refinedWith(tpnme.MirroredLabel, TypeAlias(ConstantType(Constant(label.toString))))

  /** A path referencing the companion of class type `clsType` */
  private def companionPath(clsType: Type, span: Span)(using Context) =
    val ref = pathFor(clsType.mirrorCompanionRef)
    assert(ref.symbol.is(Module) && (clsType.classSymbol.is(ModuleClass) || (ref.symbol.companionClass == clsType.classSymbol)))
    ref.withSpan(span)

  private def checkFormal(formal: Type)(using Context): Boolean =
    @tailrec
    def loop(tp: Type): Boolean = tp match
      case RefinedType(parent, _, _: TypeBounds) => loop(parent)
      case RefinedType(_, _, _) => false
      case _ => true
    loop(formal)

  private def checkRefinement(formal: Type, name: TypeName, expected: Type, span: Span)(using Context): Unit =
    val actual = formal.lookupRefined(name)
    if actual.exists && !(expected =:= actual)
    then report.error(
      em"$name mismatch, expected: $expected, found: $actual.", ctx.source.atSpan(span))

  private def mkMirroredMonoType(mirroredType: HKTypeLambda)(using Context): Type =
    val monoMap = new TypeMap:
      def apply(t: Type) = t match
        case TypeParamRef(lambda, n) if lambda eq mirroredType => mirroredType.paramInfos(n)
        case t => mapOver(t)
    monoMap(mirroredType.resultType)

  private def productMirror(mirroredType: Type, formal: Type, span: Span)(using Context): TreeWithErrors =

    /** do all parts match the class symbol? */
    def acceptable(tp: Type, cls: Symbol): Boolean = tp match
      case tp: HKTypeLambda if tp.resultType.isInstanceOf[HKTypeLambda] => false
      case tp: TypeProxy    => acceptable(tp.underlying, cls)
      case OrType(tp1, tp2) => acceptable(tp1, cls) && acceptable(tp2, cls)
      case _                => tp.classSymbol eq cls

    /** for a case class, if it will have an anonymous mirror,
     *  check that its constructor can be accessed
     *  from the calling scope.
     */
    def canAccessCtor(cls: Symbol): Boolean =
      !genAnonyousMirror(cls) || {
        def isAccessible(sym: Symbol): Boolean = ctx.owner.isContainedIn(sym)
        def isSub(sym: Symbol): Boolean = ctx.owner.ownersIterator.exists(_.derivesFrom(sym))
        val ctor = cls.primaryConstructor
        (!ctor.isOneOf(Private | Protected) || isSub(cls)) // we cant access the ctor because we do not extend cls
        && (!ctor.privateWithin.exists || isAccessible(ctor.privateWithin)) // check scope is compatible
      }

    def genAnonyousMirror(cls: Symbol): Boolean =
      cls.is(Scala2x) || cls.linkedClass.is(Case)

    def makeProductMirror(cls: Symbol): TreeWithErrors =
      val accessors = cls.caseAccessors.filterNot(_.isAllOf(PrivateLocal))
      val elemLabels = accessors.map(acc => ConstantType(Constant(acc.name.toString)))
      val nestedPairs = TypeOps.nestedPairs(accessors.map(mirroredType.resultType.memberInfo(_).widenExpr))
      val (monoType, elemsType) = mirroredType match
        case mirroredType: HKTypeLambda =>
          (mkMirroredMonoType(mirroredType), mirroredType.derivedLambdaType(resType = nestedPairs))
        case _ =>
          (mirroredType, nestedPairs)
      val elemsLabels = TypeOps.nestedPairs(elemLabels)
      checkRefinement(formal, tpnme.MirroredElemTypes, elemsType, span)
      checkRefinement(formal, tpnme.MirroredElemLabels, elemsLabels, span)
      val mirrorType =
        mirrorCore(defn.Mirror_ProductClass, monoType, mirroredType, cls.name, formal)
          .refinedWith(tpnme.MirroredElemTypes, TypeAlias(elemsType))
          .refinedWith(tpnme.MirroredElemLabels, TypeAlias(elemsLabels))
      val mirrorRef =
        if (genAnonyousMirror(cls)) anonymousMirror(monoType, ExtendsProductMirror, span)
        else companionPath(mirroredType, span)
      withNoErrors(mirrorRef.cast(mirrorType))
    end makeProductMirror

    def getErrors(cls: Symbol): List[String] = 
      if !cls.isGenericProduct then
        List(cls.whyNotGenericProduct)
      else if !canAccessCtor(cls) then
        List(i"Constructor of $cls is unnaccessible from the calling scope.")
      else 
        List.empty
    end getErrors

    mirroredType match
      case AndType(tp1, tp2) =>
        orElse(productMirror(tp1, formal, span), productMirror(tp2, formal, span))
      case _ =>
        if mirroredType.termSymbol.is(CaseVal) then
          val module = mirroredType.termSymbol
          val modulePath = pathFor(mirroredType).withSpan(span)
          if module.info.classSymbol.is(Scala2x) then
            val mirrorType = mirrorCore(defn.Mirror_SingletonProxyClass, mirroredType, mirroredType, module.name, formal)
            val mirrorRef = New(defn.Mirror_SingletonProxyClass.typeRef, modulePath :: Nil)
            withNoErrors(mirrorRef.cast(mirrorType))
          else
            val mirrorType = mirrorCore(defn.Mirror_SingletonClass, mirroredType, mirroredType, module.name, formal)
            withNoErrors(modulePath.cast(mirrorType))
        else
          val cls = mirroredType.classSymbol
          if acceptable(mirroredType, cls)
            && cls.isGenericProduct
            && canAccessCtor(cls)
          then
            makeProductMirror(cls)
          else
            (EmptyTree, getErrors(cls))
  end productMirror

  private def sumMirror(mirroredType: Type, formal: Type, span: Span)(using Context): TreeWithErrors =

    val cls = mirroredType.classSymbol
    val useCompanion = cls.useCompanionAsSumMirror
    val declScope = if useCompanion then cls.linkedClass else ctx.owner
    val clsIsGenericSum = cls.isGenericSum(declScope)

    def acceptable(tp: Type): Boolean = tp match
      case tp: TermRef => false
      case tp: HKTypeLambda if tp.resultType.isInstanceOf[HKTypeLambda] => false
      case tp: TypeProxy => acceptable(tp.underlying)
      case OrType(tp1, tp2) => acceptable(tp1) && acceptable(tp2)
      case _            => tp.classSymbol eq cls

    if acceptable(mirroredType) && clsIsGenericSum then
      val elemLabels = cls.children.map(c => ConstantType(Constant(c.name.toString)))

      def solve(sym: Symbol): Type = sym match
        case childClass: ClassSymbol =>
          assert(childClass.isOneOf(Case | Sealed))
          if childClass.is(Module) then
            childClass.sourceModule.termRef
          else
            childClass.primaryConstructor.info match
              case info: PolyType =>
                // Compute the the full child type by solving the subtype constraint
                // `C[X1, ..., Xn] <: P`, where
                //
                //   - P is the current `mirroredType`
                //   - C is the child class, with type parameters X1, ..., Xn
                //
                // Contravariant type parameters are minimized, all other type parameters are maximized.
                def instantiate(using Context) =
                  val poly = constrained(info, untpd.EmptyTree)._1
                  val resType = poly.finalResultType
                  val target = mirroredType match
                    case tp: HKTypeLambda => tp.resultType
                    case tp => tp
                  resType <:< target
                  val tparams = poly.paramRefs
                  val variances = childClass.typeParams.map(_.paramVarianceSign)
                  val instanceTypes = tparams.lazyZip(variances).map((tparam, variance) =>
                    TypeComparer.instanceType(tparam, fromBelow = variance < 0))
                  resType.substParams(poly, instanceTypes)
                instantiate(using ctx.fresh.setExploreTyperState().setOwner(childClass))
              case _ =>
                childClass.typeRef
        case child => child.termRef
      end solve

      val (monoType, elemsType) = mirroredType match
        case mirroredType: HKTypeLambda =>
          val elems = mirroredType.derivedLambdaType(
            resType = TypeOps.nestedPairs(cls.children.map(solve))
          )
          (mkMirroredMonoType(mirroredType), elems)
        case _ =>
          val elems = TypeOps.nestedPairs(cls.children.map(solve))
          (mirroredType, elems)

      val mirrorType =
        mirrorCore(defn.Mirror_SumClass, monoType, mirroredType, cls.name, formal)
            .refinedWith(tpnme.MirroredElemTypes, TypeAlias(elemsType))
            .refinedWith(tpnme.MirroredElemLabels, TypeAlias(TypeOps.nestedPairs(elemLabels)))
      val mirrorRef =
        if useCompanion then companionPath(mirroredType, span)
        else anonymousMirror(monoType, ExtendsSumMirror, span)
      withNoErrors(mirrorRef.cast(mirrorType))
    else if !clsIsGenericSum then
      (EmptyTree, List(cls.whyNotGenericSum(declScope)))
    else 
      EmptyTreeNoError
  end sumMirror

  def makeMirror
      (synth: (Type, Type, Span) => Context ?=> TreeWithErrors, formal: Type, span: Span)
      (using Context): TreeWithErrors =
    if checkFormal(formal) then
      formal.member(tpnme.MirroredType).info match
        case TypeBounds(mirroredType, _) => synth(TypeOps.stripTypeVars(mirroredType), formal, span)
        case other => EmptyTreeNoError
    else EmptyTreeNoError

  /** An implied instance for a type of the form `Mirror.Product { type MirroredType = T }`
   *  where `T` is a generic product type or a case object or an enum case.
   */
  val synthesizedProductMirror: SpecialHandler = (formal, span) =>
    makeMirror(productMirror, formal, span)

  /** An implied instance for a type of the form `Mirror.Sum { type MirroredType = T }`
   *  where `T` is a generic sum type.
   */
  val synthesizedSumMirror: SpecialHandler = (formal, span) =>
    makeMirror(sumMirror, formal, span)

  /** An implied instance for a type of the form `Mirror { type MirroredType = T }`
   *  where `T` is a generic sum or product or singleton type.
   */
  val synthesizedMirror: SpecialHandler = (formal, span) =>
    formal.member(tpnme.MirroredType).info match
      case TypeBounds(mirroredType, _) =>
        if mirroredType.termSymbol.is(CaseVal)
           || mirroredType.classSymbol.isGenericProduct
        then
          synthesizedProductMirror(formal, span)
        else
          synthesizedSumMirror(formal, span)
      case _ => EmptyTreeNoError

  private def escapeJavaArray(tp: Type)(using Context): Type = tp match
    case JavaArrayType(elemTp) => defn.ArrayOf(escapeJavaArray(elemTp))
    case _                     => tp

  private enum ManifestKind:
    case Full, Opt, Clss

    /** The kind that should be used for an array element, if we are `OptManifest` then this
     *  prevents wildcards arguments of Arrays being converted to `NoManifest`
     */
    def arrayElem = if this == Full then this else Clss

  end ManifestKind

  /** Manifest factory that does enough to satisfy the equality semantics for
   *  - `scala.reflect.OptManifest` (only runtime class is recorded)
   *  - `scala.reflect.Manifest` (runtime class of arguments are recorded, with wildcard upper bounds wrapped)
   *  however,`toString` may be different.
   *
   * There are some differences to `ClassTag`,
   *  e.g. in Scala 2 `manifest[Int @unchecked]` will fail, but `classTag[Int @unchecked]` succeeds.
   */
  private def manifestFactoryOf(kind: ManifestKind): SpecialHandler = (formal, span) =>
    import ManifestKind.*

    /* Creates a tree that calls the factory method called constructor in object scala.reflect.Manifest */
    def factoryManifest(constructor: TermName, tparg: Type, args: Tree*): Tree =
      if args.contains(EmptyTree) then
        EmptyTree
      else
        val factory = if kind == Full then defn.ManifestFactoryModule else defn.ClassManifestFactoryModule
        applyOverloaded(ref(factory), constructor, args.toList, tparg :: Nil, Types.WildcardType).withSpan(span)

    /* Creates a tree representing one of the singleton manifests.*/
    def singletonManifest(name: TermName) =
      ref(defn.ManifestFactoryModule).select(name).ensureApplied.withSpan(span)

    def synthArrayManifest(elemTp: Type, kind: ManifestKind, topLevel: Boolean): Tree =
      factoryManifest(nme.arrayType, elemTp, synthesize(elemTp, kind.arrayElem, topLevel))

    /** manifests generated from wildcards can not equal Int,Long,Any,AnyRef,AnyVal etc,
     *  so we wrap their upper bound.
     */
    def synthWildcardManifest(tp: Manifestable, hi: Type, topLevel: Boolean): Tree =
      factoryManifest(nme.wildcardType, tp, singletonManifest(nme.Nothing), synthesize(hi, Full, topLevel))

    /** `Nil` if not full manifest */
    def synthArgManifests(tp: Manifestable): List[Tree] = tp match
      case AppliedType(_, args) if kind == Full && tp.typeSymbol.isClass =>
        args.map(synthesize(_, Full, topLevel = false))
      case _ =>
        Nil

    /** This type contains all top-level types supported by Scala 2's algorithm */
    type Manifestable =
      ThisType | TermRef | ConstantType | TypeRef | AppliedType | TypeBounds | RecType | RefinedType | AndType

    def canManifest(tp: Manifestable, topLevel: Boolean) =
      val sym = tp.typeSymbol
      !sym.isAbstractType
      && hasStableErasure(tp)
      && !(topLevel && defn.isBottomClassAfterErasure(sym))

    /** adapted from `syntheticClassTag` */
    def synthManifest(tp: Manifestable, kind: ManifestKind, topLevel: Boolean) = tp match
      case defn.ArrayOf(elemTp)              => synthArrayManifest(elemTp, kind, topLevel)
      case TypeBounds(_, hi) if kind == Full => synthWildcardManifest(tp, hi, topLevel)

      case tp if canManifest(tp, topLevel) =>
        val sym = tp.typeSymbol
        if sym.isPrimitiveValueClass || defn.SpecialManifestClasses.contains(sym) then
          singletonManifest(sym.name.toTermName)
        else
          erasure(tp) match
            case JavaArrayType(elemTp) =>
              synthArrayManifest(escapeJavaArray(elemTp), kind, topLevel)

            case etp =>
              val clsArg = clsOf(etp).asInstance(defn.ClassType(tp)) // cast needed to resolve overloading
              factoryManifest(nme.classType, tp, (clsArg :: synthArgManifests(tp))*)

      case _ =>
        EmptyTree

    end synthManifest

    def manifestOfType(tp0: Type, kind: ManifestKind, topLevel: Boolean): Tree = tp0.dealiasKeepAnnots match
      case tp1: Manifestable => synthManifest(tp1, kind, topLevel)
      case tp1               => EmptyTree

    def synthesize(tp: Type, kind: ManifestKind, topLevel: Boolean): Tree =
      manifestOfType(tp, kind, topLevel) match
        case EmptyTree if kind == Opt => ref(defn.NoManifestModule)
        case result                   => result

    formal.argInfos match
      case arg :: Nil =>
        val manifest = synthesize(fullyDefinedType(arg, "Manifest argument", span), kind, topLevel = true)
        if manifest != EmptyTree then
          report.deprecationWarning(
            i"""Compiler synthesis of Manifest and OptManifest is deprecated, instead
               |replace with the type `scala.reflect.ClassTag[$arg]`.
               |Alternatively, consider using the new metaprogramming features of Scala 3,
               |see https://docs.scala-lang.org/scala3/reference/metaprogramming.html""", ctx.source.atSpan(span))
        withNoErrors(manifest)
      case _ =>
        EmptyTreeNoError

  end manifestFactoryOf

  val synthesizedManifest: SpecialHandler = manifestFactoryOf(ManifestKind.Full)
  val synthesizedOptManifest: SpecialHandler = manifestFactoryOf(ManifestKind.Opt)

  val specialHandlers = List(
    defn.ClassTagClass        -> synthesizedClassTag,
    defn.TypeTestClass        -> synthesizedTypeTest,
    defn.CanEqualClass        -> synthesizedCanEqual,
    defn.ValueOfClass         -> synthesizedValueOf,
    defn.TupledFunctionClass  -> synthesizedTupleFunction,
    defn.Mirror_ProductClass  -> synthesizedProductMirror,
    defn.Mirror_SumClass      -> synthesizedSumMirror,
    defn.MirrorClass          -> synthesizedMirror,
    defn.ManifestClass        -> synthesizedManifest,
    defn.OptManifestClass     -> synthesizedOptManifest,
  )

  def tryAll(formal: Type, span: Span)(using Context): TreeWithErrors =
    def recur(handlers: SpecialHandlers): TreeWithErrors = handlers match
      case (cls, handler) :: rest =>
        def baseWithRefinements(tp: Type): Type = tp.dealias match
          case tp @ RefinedType(parent, rname, rinfo) =>
            tp.derivedRefinedType(baseWithRefinements(parent), rname, rinfo)
          case _ =>
            tp.baseType(cls)
        val base = baseWithRefinements(formal)
        val result =
          if (base <:< formal.widenExpr)  
            // With the subtype test we enforce that the searched type `formal` is of the right form
            handler(base, span)
          else EmptyTreeNoError
        orElse(result, recur(rest))
      case Nil =>
        EmptyTreeNoError
    recur(specialHandlers)

end Synthesizer

object Synthesizer:

  /** Tuple used to store the synthesis result with a list of errors */ 
  type TreeWithErrors = (Tree, List[String])
  private def withNoErrors(tree: Tree): TreeWithErrors = (tree, List.empty)

  private val EmptyTreeNoError: TreeWithErrors = withNoErrors(EmptyTree)
  private def orElse(treeWithErrors1: TreeWithErrors, treeWithErrors2: => TreeWithErrors): TreeWithErrors = treeWithErrors1 match
    case (tree, errors) if tree eq genericEmptyTree => (treeWithErrors2._1, treeWithErrors2._2 ::: errors)
    case _                                          => treeWithErrors1
end Synthesizer
