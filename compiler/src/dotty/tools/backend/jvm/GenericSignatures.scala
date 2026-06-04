package dotty.tools
package backend
package jvm

import dotty.tools.dotc.core.Annotations.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.core.Definitions
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.TypeApplications.*
import dotty.tools.dotc.core.TypeErasure.*
import dotty.tools.dotc.core.classfile.ClassfileConstants
import dotty.tools.dotc.transform.ValueClasses

import java.lang.StringBuilder
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** Helper object to generate generic java signatures, as defined in
 *  the Java Virtual Machine Specification, §4.3.4
 */
object GenericSignatures {

  /** Generate the signature for `sym0`, with type `info`, as defined in
   *  the Java Virtual Machine Specification, §4.3.4
   *
   *  @param sym0 The symbol for which to define the signature
   *  @param info The type of the symbol
   *  @return The signature if it could be generated, `null` otherwise.
   */
  def javaSig(sym0: Symbol, info: Type)(using Context): StringBuilder | Null =
    if mayNeedSignature(sym0, info) then atPhase(erasurePhase)(javaSig0(sym0, info))
    else null

  private def mayNeedSignature(sym0: Symbol, info: Type)(using Context) = {
    def mayNeedSignature(t: Type): Boolean = t match
      case ExprType(e) => mayNeedSignature(e)
      case MethodTpe(_, ps, res) => (!sym0.isConstructor && mayNeedSignature(res)) || ps.exists(mayNeedSignature)
      case _: TypeRef => !t.isAny && !t.isAnyRef && !t.isRef(defn.StringClass, skipRefined = false) && !t.isPrimitiveValueType
      case _ => true

    // Non-class local symbols definitely don't need one, they're not observable
    if sym0.isLocal && !sym0.isClass then false
    else mayNeedSignature(info)
  }

  private def javaSig0(sym0: Symbol, info: Type)(using Context): StringBuilder | Null = {
    // This works as long as mangled names are always valid Java identifiers (see git history of this method).
    def sanitizeName(name: Name): String = name.mangledString

    val builder = new StringBuilder(64)

    // Track class type parameter names that are shadowed by method type parameters
    // Used to trigger renaming of method type parameters to avoid conflicts
    val shadowedClassTypeParamNames = collection.mutable.Set.empty[String]
    val methodTypeParamRenaming = collection.mutable.Map.empty[String, String]

    def freshTypeParamName(sanitizedName: String): String = {
      if !shadowedClassTypeParamNames.contains(sanitizedName) then sanitizedName
      else {
        var i = 1
        var newName = sanitizedName + i
        while shadowedClassTypeParamNames.contains(newName) do
          i += 1
          newName = sanitizedName + i
        methodTypeParamRenaming(sanitizedName) = newName
        shadowedClassTypeParamNames += newName
        newName
      }
    }

    def superSig(cls: Symbol, parents: List[Type]): Unit = {
      def isInterfaceOrTrait(sym: Symbol) = sym.is(PureInterface) || sym.is(Trait)

      val minParents = minimizeParents(cls, parents)
      val validParents =
        if sym0.enclosingClass.is(Trait) then
          // java is unthrilled about seeing interfaces inherit from classes
          minParents filter (p => isInterfaceOrTrait(p.classSymbol))
        else minParents

      // a signature should always start with a class
      validParents.headOption match
        case None => jsig(defn.ObjectType)
        case Some(head) if isInterfaceOrTrait(head.typeSymbol) => jsig(defn.ObjectType)
        case _ => ()
      validParents.foreach(boxedSig)
    }

    inline def boxedSig(tp: Type): Unit = jsig(tp.widenDealias, vcBoxing = ValueClassBoxing.Box)

    /** The signature of the upper-bound of a type parameter.
     *
     *  @note precondition: none of the bounds are themselves type parameters.
     *       TODO: Remove this restriction so we can support things like:
     *
     *           class Foo[A]:
     *              def foo[S <: A & Object](...): ...
     *
     *        Which should emit a signature `S <: A`. See the handling
     *        of `AndType` in `jsig` which already supports `def foo(x: A & Object)`.
     */
    def boundsSig(bounds: List[Type]): Unit = {
      val (repr :: _, others) = splitIntersection(bounds): @unchecked
      builder.append(':')

      // In Java, intersections always erase to their first member, so put
      // whatever parent erases to the Scala intersection erasure first in the
      // signature.
      if repr.classSymbol.is(Trait) then
        // An initial ':' is needed if the intersection starts with an interface
        // (cf https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-TypeParameter)
        builder.append(':')
      boxedSig(repr)
      others.filter(_.classSymbol.is(Trait)).foreach { tp =>
        builder.append(':')
        boxedSig(tp)
      }
    }

    /** The parents of this intersection where type parameters
     *  that cannot appear in the signature have been replaced
     *  by their upper-bound.
     */
    def flattenedIntersection(tp: AndType)(using Context): Iterable[Type] =
      val parents = ListBuffer[Type]()

      def collect(parent: Type, parents: ListBuffer[Type]): Unit = parent.widenDealias match
        case AndType(tp1, tp2) =>
          collect(tp1, parents)
          collect(tp2, parents)
        case parent: TypeRef =>
          if parent.symbol.isClass || isTypeParameterInSig(parent.symbol, sym0) then
            parents += parent
          else
            collect(parent.superType, parents)
        case parent =>
          parents += parent

      collect(tp, parents)
      parents
    end flattenedIntersection

    /** Split the `parents` of an intersection into two subsets:
     *  those whose individual erasure matches the overall erasure
     *  of the intersection and the others.
     */
    def splitIntersection(parents: Iterable[Type])(using Context): (Iterable[Type], Iterable[Type]) =
      val erasedParents = parents.map(erasure)
      val erasedTp = erasedGlb(erasedParents)
      parents.zip(erasedParents)
        .partitionMap((parent, erasedParent) =>
          if erasedParent =:= erasedTp then
            Left(parent)
          else
            Right(parent))

    def tparamSig(param: TypeParamInfo): Unit = {
      val freshName = freshTypeParamName(sanitizeName(param.paramName.lastPart))
      builder.append(freshName)
      boundsSig(hiBounds(param.paramInfo.bounds))
    }

    def polyParamSig(tparams: Iterable[TypeParamInfo]): Unit =
      if (tparams.nonEmpty) {
        builder.append('<')
        tparams.foreach(tparamSig)
        builder.append('>')
      }

    def typeParamSig(name: Name): Unit = {
      builder.append(ClassfileConstants.TVAR_TAG)
      builder.append(sanitizeName(name))
      builder.append(';')
    }

    def typeParamSigWithName(sanitizedName: String): Unit = {
      builder.append(ClassfileConstants.TVAR_TAG)
      builder.append(sanitizedName)
      builder.append(';')
    }

    def methodResultSig(restpe: Type): Unit = {
      val finalType = restpe.finalResultType
      val sym = finalType.typeSymbol
      if (sym == defn.UnitClass || sym == defn.BoxedUnitModule || sym0.isConstructor)
        builder.append(ClassfileConstants.VOID_TAG)
      else
        jsig(finalType)
    }

    def classSig(sym: ClassSymbol, pre: Type = NoType, args: List[Type] = Nil): Unit = {
      def argSig(tp: Type): Unit =
        tp.dealias match {
          case bounds: TypeBounds =>
            if (!(defn.AnyType <:< bounds.hi)) {
              builder.append('+')
              boxedSig(bounds.hi)
            }
            else if (!(bounds.lo <:< defn.NothingType)) {
              builder.append('-')
              boxedSig(bounds.lo)
            }
            else builder.append('*')
          case hkt: HKTypeLambda =>
            hkt.resultType match
              case a: AppliedType =>
                if hkt.paramInfos.forall(i => i.lo.isNothingType && i.hi.isAny) then
                  // For unbounded arguments, instead of emitting `X<j.l.Object>`,
                  // emit just `X` as a raw type if it's a class;
                  // this helps with Java compat in cases where the exact generic arguments were erased
                  if a.tycon.dealias.typeSymbol.isClass then
                    jsig(a.tycon)
                  else
                    // but if it's an HKT, we cannot represent that in a Java generic signature, so emit a wildcard
                    builder.append("*")
                else
                  // For bounded arguments, we can't translate it cleanly so emit an erased type
                  jsig(erasure(a.tycon))
              case res =>
                // value classes cannot appear as generic arguments
                jsig(res, vcBoxing = ValueClassBoxing.Box)
          case _ =>
            boxedSig(tp.widenDealias.widenNullaryMethod)
              // `tp` might be a singleton type referring to a getter.
              // Hence the widenNullaryMethod.
        }

      pre.widenDealias match {
        // If the class is an inner class of a generic class, we must emit the outer generic class with its parameters
        // (see test `inner-of-generic` for an example of Java compatibility)
        case RefOrAppliedType(preSym: ClassSymbol, prePre, preArgs) if preArgs.nonEmpty =>
          classSig(preSym, prePre, preArgs)
          builder.replace(builder.length() - 1, builder.length(), ".") // instead of ending the outer name with ';', we add an inner name
          builder.append(sanitizeName(sym.targetName))
        // For the rest, we time-travel so we get the full name after inner classes have been lifted to package scope
        case _ =>
          val name = atPhase(flattenPhase.next) { sanitizeName(sym.fullName).replace('.', '/') }
          builder.append('L')
          builder.append(name)
      }
      if (args.nonEmpty) {
        builder.append('<')
        args foreach argSig
        builder.append('>')
      }
      builder.append(';')
    }

    enum ValueClassBoxing:
      case Box, Unbox, UnboxOnlyPrimitives

    def jsig(tp0: Type, toplevel: Boolean = false, vcBoxing: ValueClassBoxing = ValueClassBoxing.Unbox): Unit = {
      def arraySig(elemtp: Type): Unit =
        if (isGenericArrayElement(elemtp, isScala2 = false))
          jsig(defn.ObjectType)
        else
          builder.append(ClassfileConstants.ARRAY_TAG)
          elemtp match
            case TypeBounds(lo, hi) => jsig(hi.widenDealias)
            // derived VCs are not unboxed inside arrays,
            // i.e., `Array[VC]` where `class VC(n: X) extends AnyVal`
            // is `[LVC;`, not `[LX;`
            case _ => jsig(elemtp, vcBoxing = ValueClassBoxing.UnboxOnlyPrimitives)

      val tp = tp0.dealias
      tp match {
        case RefinedType(parent, _, _) =>
          jsig(parent, toplevel = toplevel, vcBoxing = vcBoxing)

        case ref @ TypeParamRef(_: PolyType, _) =>
          val erasedUnderlying = fullErasure(ref.underlying.bounds.hi)
          // don't emit type param name if the param is upper-bounded by a primitive type (including via a value class)
          if erasedUnderlying.isPrimitiveValueType then
            jsig(erasedUnderlying, toplevel = toplevel, vcBoxing = vcBoxing)
          else
            val name = sanitizeName(ref.paramName.lastPart)
            val nameToUse = methodTypeParamRenaming.getOrElse(name, name)
            typeParamSigWithName(nameToUse)

        case ref: TermRef if ref.symbol.isGetter =>
          // If the type of a val is a TermRef to another val, generating the generic signature
          // based on the underlying type will produce the type `scala.Function0<underlying>`
          // The reason behind this is that during the `getters` phase, the same symbol will now
          // refer to the getter where the type will be now `=> <underlying>`.
          // Since the TermRef originally intended to capture the underlying type of a `val`,
          // we recover that information by directly checking the resultType of the getter.
          // See `tests/run/i24553.scala` for an example
          jsig(ref.info.resultType, toplevel = toplevel, vcBoxing = vcBoxing)

        case ref: SingletonType =>
          // Singleton types like `x.type` need to be widened to their underlying type
          // For example, `def identity[A](x: A): x.type` should have signature
          // with return type `A` (not `java.lang.Object`)
          jsig(ref.underlying, toplevel = toplevel, vcBoxing = vcBoxing)

        case defn.ArrayOf(elemtp) =>
          arraySig(elemtp)

        case JavaArrayType(elemtp) =>
          arraySig(elemtp)

        case RefOrAppliedType(sym, pre, args) =>
          if isTypeParameterInSig(sym, sym0) then
            assert(!sym.isAliasType || sym.info.isLambdaSub, s"Unexpected alias type: $sym")
            typeParamSig(sym.targetName.lastPart)
          else defn.specialErasure.get(sym) match
            case Some(special) =>
              jsig(special.typeRef)
            case None =>
              if (sym == defn.PairClass && tupleArity(tp) > Definitions.MaxTupleArity)
                jsig(defn.TupleXXLClass.typeRef)
              else if (sym == defn.UnitClass || sym == defn.BoxedUnitModule)
                jsig(defn.BoxedUnitClass.typeRef)
              else if (sym == defn.NothingClass)
                builder.append("Lscala/runtime/Nothing$;")
              else if (sym == defn.NullClass)
                builder.append("Lscala/runtime/Null$;")
              else if (sym.isPrimitiveValueClass)
                if (vcBoxing == ValueClassBoxing.Box) jsig(defn.boxedClass(sym).typeRef)
                else if (builder.length == 0 && sym0.isField) () // field generic signatures can only be reference types (JVMS §4.7.9.1)
                else builder.append(defn.typeTag(sym.info))
              else if defn.isSyntheticFunctionClass(sym) then
                defn.functionTypeErasure(sym).classSymbol match
                  case classSym: ClassSymbol => classSig(classSym, pre, if classSym.typeParams.isEmpty then Nil else args)
                  case NoSymbol => throw new AssertionError(s"No class symbol for erased function type $sym")
              else sym match
                case classSym: ClassSymbol if classSym.isDerivedValueClass && vcBoxing == ValueClassBoxing.Unbox =>
                  val underlying = ValueClasses.underlyingOfValueClass(classSym)
                  val seenUnderlying = underlying.asSeenFrom(tp, classSym)
                  // For binary compatibility with Scala 2, as documented in TypeErasure,
                  // we need to special cases for polymorphic value classes:
                  // `Foo[X]` erases to `X` except that primitives use their boxed type,
                  // and `Bar[X]` for `class Bar[A](x: Array[A]) extends AnyVal` erases like the definition-site `Array[A]`.
                  // The end-to-end binary compatibility is checked by i8001
                  // There are more targeted tests for generic signatures at i24276 and t6344
                  val compatibleUnderlying =
                    if seenUnderlying.isPrimitiveValueType && !underlying.isPrimitiveValueType then defn.boxedType(seenUnderlying)
                    else if underlying.derivesFrom(defn.ArrayClass) then erasure(underlying)
                    else seenUnderlying
                  jsig(compatibleUnderlying, toplevel = toplevel)
                case classSym: ClassSymbol => classSig(classSym, pre, args)
                case _ => jsig(erasure(tp), toplevel = toplevel, vcBoxing = vcBoxing)

        case ExprType(restpe) =>
          if toplevel then
            builder.append("()")
            methodResultSig(restpe)
          else
            jsig(defn.FunctionType(0).appliedTo(restpe))

        case mtd: MethodOrPoly =>
          val collectTParams = toplevel && !sym0.isConstructor
          val (tparams, vparams, rte) = collectMethodParams(mtd, collectTParams)
          if (tparams != null) {
            if (sym0.is(Method)) {
              val (usedMethodTypeParamNames, usedClassTypeParams) = collectUsedTypeParams(vparams, rte, sym0)
              val methodTypeParamNames = tparams.map(tp => sanitizeName(tp.paramName.lastPart)).toSet
              // Only add class type parameters to shadowedClassTypeParamNames if they are:
              // 1. Referenced in the method signature, AND
              // 2. Shadowed by a method type parameter with the same name
              // This will trigger renaming of the method type parameter
              usedClassTypeParams.foreach { classTypeParam =>
                val classTypeParamName = sanitizeName(classTypeParam.targetName)
                if methodTypeParamNames.contains(classTypeParamName) then
                  shadowedClassTypeParamNames += classTypeParamName
              }
            }
            polyParamSig(tparams)
          }
          builder.append('(')
          for vparam <- vparams do jsig(vparam)
          builder.append(')')
          methodResultSig(rte)

        case OrNull(tp1) if !tp1.derivesFrom(defn.AnyValClass) =>
          // Special case for nullable union types whose underlying type is not a value class.
          // For example, `T | Null` where `T` is a type parameter becomes `T` in the signature;
          // `Int | Null` still becomes `Object`.
          jsig(tp1)

        case tp: AndType =>
          // Only intersections appearing as the upper-bound of a type parameter
          // can be preserved in generic signatures and those are already
          // handled by `boundsSig`, so here we fall back to picking a parent of
          // the intersection to determine its overall signature. We must pick a
          // parent whose erasure matches the erasure of the intersection
          // because javac relies on the generic signature to determine the
          // bytecode signature. Additionally, we prefer picking a type
          // parameter since that will likely lead to a more precise type.
          val parents = flattenedIntersection(tp)
          val (reprParents, _) = splitIntersection(parents)
          val repr =
            reprParents.find(_.typeSymbol.is(TypeParam)).getOrElse(reprParents.head)
          jsig(repr, toplevel = false, vcBoxing = vcBoxing)

        case ci: ClassInfo =>
          val tParams = tp.typeParams
          if (toplevel) polyParamSig(tParams)
          superSig(ci.typeSymbol, ci.parents)

        case AnnotatedType(atp, _) =>
          jsig(atp, toplevel, vcBoxing)

        case hktl: HKTypeLambda =>
          jsig(hktl.finalResultType, toplevel, vcBoxing)

        case ErasedValueType(tycon, underlying) =>
          if vcBoxing == ValueClassBoxing.Unbox || (vcBoxing == ValueClassBoxing.UnboxOnlyPrimitives && underlying.isPrimitiveValueType)
          then jsig(underlying, toplevel, vcBoxing)
          else jsig(tycon, toplevel, vcBoxing)

        case _ =>
          val etp = erasure(tp)
          assert(etp ne tp, i"$tp erases to itself")
          jsig(etp, toplevel, vcBoxing)
      }
    }
    jsig(info, toplevel = true)
    for annot <- sym0.annotations do
      annot match
        case ThrownException(e) =>
          builder.append('^')
          jsig(e, toplevel = true)
        case _ => ()

    if builder.length == 0
    then null
    else builder
  }

  /* Drop redundant types (ones which are implemented by some other parent) from the immediate parents.
   * This is important on Android because there is otherwise an interface explosion.
   */
  private def minimizeParents(cls: Symbol, parents: Iterable[Type])(using Context): Iterable[Type] = if (parents.isEmpty) parents else {
    var leaves = collection.mutable.ListBuffer.empty[Type]
    for candidate <- parents do
      val candidateSym = candidate.typeSymbol
      val required = !leaves.exists(t => t.typeSymbol.isSubClass(candidateSym))
      if (required) {
        leaves = leaves filter { t =>
          val ts = t.typeSymbol
          !(ts.is(Trait) || ts.is(PureInterface)) || !candidateSym.isSubClass(ts)
        }
        leaves += candidate
      }
    leaves
  }

  private def hiBounds(bounds: TypeBounds)(using Context): List[Type] = bounds.hi.widenDealias match {
    case AndType(tp1, tp2) => hiBounds(tp1.bounds) ::: hiBounds(tp2.bounds)
    case tp => tp :: Nil
  }


  // only refer to type params that will actually make it into the sig, this excludes:
  // * type parameters appearing in method parameters
  // * type members not visible in an enclosing template
  private def isTypeParameterInSig(sym: Symbol, initialSymbol: Symbol)(using Context) =
    !sym.maybeOwner.isTypeParam &&
      sym.isTypeParam && (
      sym.isContainedIn(initialSymbol.topLevelClass) ||
        (initialSymbol.is(Method) && initialSymbol.typeParams.contains(sym))
      )

  // @M #2585 when generating a java generic signature that includes
  // a selection of an inner class p.I, (p = `pre`, I = `cls`) must
  // rewrite to p'.I, where p' refers to the class that directly defines
  // the nested class I.
  //
  // See also #2585 marker in javaSig: there, type arguments must be
  // included (use pre.baseType(cls.owner)).
  //
  // This requires that cls.isClass.
  private def rebindInnerClass(pre: Type, cls: Symbol)(using Context): Type = {
    val owner = cls.owner
    if (owner.is(PackageClass) || owner.isTerm) pre else cls.owner.info /* .tpe_* */
  }

  private object RefOrAppliedType {
    private enum ResolvedAppliedType:
      case Resolved(t: Type)
      case NotResolved
      case Bail
    // In the special case where we see a type parameter applied to type parameters,
    // such as `K[X, Y]` given `[X, Y, K <: Iterable[(X, Y)]]`, we must find its bound
    // and instantiate it, otherwise in our example we end up with `Iterable[X, Y]` which is nonsensical.
    private def resolveAppliedType(a: AppliedType)(using Context): ResolvedAppliedType =
      a.tycon match
        case TypeParamRef(binder, paramNum) =>
          binder.paramInfos(paramNum).hi match
            case hkt @ HKTypeLambda(_, _) =>
              val instantiated = hkt.instantiate(a.args).dealias
              // However, since Java doesn't have a way to refer to HKTs in generic signatures,
              // we must trade precision for termination by only resolving one level,
              // otherwise we end up in infinite loops,
              // e.g., in `X[A] <: Thing[X[A]]` or `X[A] <: X[Thing[A]]` we keep resolving `X`.
              // In that case we must completely give up on the genericity, i.e.,
              // in `X[A] <: Y[X[Z[A]]]` it would not be correct to use `Y[A]` as a type signature! 
              if instantiated.existsPart(_ == a.tycon) then ResolvedAppliedType.Bail
              else ResolvedAppliedType.Resolved(instantiated)
            case _ => ResolvedAppliedType.NotResolved
        case _ => ResolvedAppliedType.NotResolved

    @tailrec
    def unapply(tp: Type)(using Context): Option[(Symbol, Type, List[Type])] = tp match
      case TypeRef(pre, _) if !tp.typeSymbol.isAliasType =>
        Some((tp.typeSymbol, pre, Nil))
      case TypeParamRef(_, _) =>
        Some((tp.typeSymbol, tp, Nil))
      case TermParamRef(_, _) =>
        Some((tp.termSymbol, tp, Nil))
      case a @ AppliedType(pre, args) =>
        resolveAppliedType(a) match
          case ResolvedAppliedType.Resolved(resolved) => unapply(resolved)
          case ResolvedAppliedType.NotResolved => Some((pre.typeSymbol, pre, args))
          case ResolvedAppliedType.Bail => None
      case _ =>
        None
  }

  private def collectMethodParams(mtd: MethodOrPoly, collectTParams: Boolean)(using Context): (Iterable[TypeParamInfo] | Null, Iterable[Type], Type) =
    val tparams = if collectTParams then ListBuffer.empty[TypeParamInfo] else null
    val vparams = ListBuffer.empty[Type]

    @tailrec def recur(tpe: Type): Type = tpe match
      case mtd: MethodType =>
        vparams ++= mtd.paramInfos.filterNot(_.hasAnnotation(defn.ErasedParamAnnot))
        mtd.resType.dealias match
          // Returned context functions are erased by putting their parameters into the method's parameters,
          // so we must duplicate that logic here
          case AppliedType(tycon, args) if tycon.typeSymbol.name.isContextFunction =>
            vparams ++= args.take(args.length - 1)
            recur(args.last)
          // Handle erased types too
          case defn.FunctionTypeOfMethod(mt: MethodType) if mt.isContextualMethod =>
            recur(mt)
          case _ =>
            recur(mtd.resType)
      case PolyType(tps, tpe) =>
        if tparams != null then tparams ++= tps
        recur(tpe)
      case _ =>
        tpe
    end recur

    val rte = recur(mtd)
    (tparams, vparams, rte)
  end collectMethodParams

  /** Collect type parameters that are actually used in the given types. */
  private def collectUsedTypeParams(types: Iterable[Type], resType: Type, initialSymbol: Symbol)(using Context): (Set[Name], Set[Symbol]) =
    def isTypeParameterInMethSig(sym: Symbol, initialSymbol: Symbol)(using Context) =
      !sym.maybeOwner.isTypeParam && // check if it's not higher order type param
        sym.isTypeParam && sym.owner == initialSymbol

    val usedMethodTypeParamNames = collection.mutable.Set.empty[Name]
    val usedClassTypeParams = collection.mutable.Set.empty[Symbol]

    def collect(tp: Type): Unit = tp.foreachPart:
      case ref @ TypeParamRef(_: PolyType, _) =>
        usedMethodTypeParamNames += ref.paramName
      case tp: TypeRef =>
        val sym = tp.typeSymbol
        if sym.isTypeParam && sym.isContainedIn(initialSymbol.topLevelClass) then
          usedClassTypeParams += sym
      case _ =>

    types.foreach(collect)
    collect(resType)
    (usedMethodTypeParamNames.toSet, usedClassTypeParams.toSet)
  end collectUsedTypeParams
}
