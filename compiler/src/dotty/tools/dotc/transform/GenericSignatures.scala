package dotty.tools
package dotc
package transform

import core.Annotations.*
import core.Contexts.*
import core.Phases.*
import core.Decorators.*
import core.Definitions
import core.Flags.*
import core.Names.Name
import core.Symbols.*
import core.TypeApplications.{EtaExpansion, TypeParamInfo}
import core.TypeErasure.{erasedGlb, erasure, fullErasure, isGenericArrayElement, tupleArity}
import core.Types.*
import core.classfile.ClassfileConstants

import config.Printers.transforms
import reporting.trace
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
   *  @return The signature if it could be generated, `None` otherwise.
   */
  def javaSig(sym0: Symbol, info: Type)(using Context): Option[String] =
    // Avoid generating a signature for non-class local symbols.
    if (sym0.isLocal && !sym0.isClass) None
    else atPhase(erasurePhase)(javaSig0(sym0, info))

  @noinline
  private final def javaSig0(sym0: Symbol, info: Type)(using Context): Option[String] = {
    // This works as long as mangled names are always valid valid Java identifiers,
    // if we change our name encoding, we'll have to `throw new UnknownSig` here for
    // names which are not valid Java identifiers (see git history of this method).
    def sanitizeName(name: Name): String = name.mangledString

    val builder = new StringBuilder(64)
    val isTraitSignature = sym0.enclosingClass.is(Trait)

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

      // a signature should always start with a class
      def ensureClassAsFirstParent(tps: List[Type]) = tps match {
        case Nil => defn.ObjectType :: Nil
        case head :: tail if isInterfaceOrTrait(head.typeSymbol) => defn.ObjectType :: tps
        case _ => tps
      }

      val minParents = minimizeParents(cls, parents)
      val validParents =
        if (isTraitSignature)
          // java is unthrilled about seeing interfaces inherit from classes
          minParents filter (p => isInterfaceOrTrait(p.classSymbol))
        else minParents

      val ps = ensureClassAsFirstParent(validParents)
      ps.foreach(boxedSig)
    }

    def boxedSig(tp: Type): Unit = jsig(tp.widenDealias, unboxedVCs = false)

    /** The signature of the upper-bound of a type parameter.
     *
     *  @pre none of the bounds are themselves type parameters.
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
    def flattenedIntersection(tp: AndType)(using Context): List[Type] =
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
      parents.toList
    end flattenedIntersection

    /** Split the `parents` of an intersection into two subsets:
     *  those whose individual erasure matches the overall erasure
     *  of the intersection and the others.
     */
    def splitIntersection(parents: List[Type])(using Context): (List[Type], List[Type]) =
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

    def polyParamSig(tparams: List[TypeParamInfo]): Unit =
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

    // Anything which could conceivably be a module (i.e. isn't known to be
    // a type parameter or similar) must go through here or the signature is
    // likely to end up with Foo<T>.Empty where it needs Foo<T>.Empty$.
    def fullNameInSig(sym: Symbol): Unit = {
      assert(sym.isClass)
      val name = atPhase(genBCodePhase) { sanitizeName(sym.fullName).replace('.', '/') }
      builder.append('L').append(name)
    }

    def classSig(sym: Symbol, pre: Type = NoType, args: List[Type] = Nil): Unit = {
      def argSig(tp: Type): Unit =
        tp match {
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
          case EtaExpansion(tp) =>
            argSig(tp)
          case _: HKTypeLambda =>
            builder.append('*')
          case _ =>
            boxedSig(tp.widenDealias.widenNullaryMethod)
              // `tp` might be a singleton type referring to a getter.
              // Hence the widenNullaryMethod.
        }

      if (pre.exists) {
        val preRebound = pre.baseType(sym.owner) // #2585
        if (needsJavaSig(preRebound, Nil)) {
          val i = builder.length()
          jsig(preRebound)
          if (builder.charAt(i) == 'L') {
            builder.delete(builder.length() - 1, builder.length())// delete ';'
                                                                  // If the prefix is a module, drop the '$'. Classes (or modules) nested in modules
                                                                  // are separated by a single '$' in the filename: `object o { object i }` is o$i$.
            if (preRebound.typeSymbol.is(ModuleClass))
              builder.delete(builder.length() - 1, builder.length())

            // Ensure every '.' in the generated signature immediately follows
            // a close angle bracket '>'.  Any which do not are replaced with '$'.
            // This arises due to multiply nested classes in the face of the
            // rewriting explained at rebindInnerClass.

            // TODO revisit this. Does it align with javac for code that can be expressed in both languages?
            val delimiter = if (builder.charAt(builder.length() - 1) == '>') '.' else '$'
            builder.append(delimiter).append(sanitizeName(sym.name))
          }
          else fullNameInSig(sym)
        }
        else fullNameInSig(sym)
      }
      else fullNameInSig(sym)

      if (args.nonEmpty) {
        builder.append('<')
        args foreach argSig
        builder.append('>')
      }
      builder.append(';')
    }

    @noinline
    def jsig(tp0: Type, toplevel: Boolean = false, unboxedVCs: Boolean = true): Unit = {
      inline def jsig1(tp0: Type): Unit = jsig(tp0, toplevel = false, unboxedVCs = true)

      val tp = tp0.dealias
      tp match {

        case ref @ TypeParamRef(_: PolyType, _) =>
          val erasedUnderlying = fullErasure(ref.underlying.bounds.hi)
          // don't emit type param name if the param is upper-bounded by a primitive type (including via a value class)
          if erasedUnderlying.isPrimitiveValueType then
            jsig(erasedUnderlying, toplevel = toplevel, unboxedVCs = unboxedVCs)
          else {
            val name = sanitizeName(ref.paramName.lastPart)
            val nameToUse = methodTypeParamRenaming.getOrElse(name, name)
            typeParamSigWithName(nameToUse)
          }

        case ref: TermRef if ref.symbol.isGetter =>
          // If the type of a val is a TermRef to another val, generating the generic signature
          // based on the underlying type will produce the type `scala.Function0<underlying>`
          // The reason behind this is that during the `getters` phase, the same symbol will now
          // refer to the getter where the type will be now `=> <underlying>`.
          // Since the TermRef originally intended to capture the underlying type of a `val`,
          // we recover that information by directly checking the resultType of the getter.
          // See `tests/run/i24553.scala` for an example
          jsig(ref.info.resultType, toplevel = toplevel, unboxedVCs = unboxedVCs)

        case ref: SingletonType =>
          // Singleton types like `x.type` need to be widened to their underlying type
          // For example, `def identity[A](x: A): x.type` should have signature
          // with return type `A` (not `java.lang.Object`)
          jsig(ref.underlying, toplevel = toplevel, unboxedVCs = unboxedVCs)

        case defn.ArrayOf(elemtp) =>
          if (isGenericArrayElement(elemtp, isScala2 = false))
            jsig1(defn.ObjectType)
          else
            builder.append(ClassfileConstants.ARRAY_TAG)
            elemtp match
              case TypeBounds(lo, hi) => jsig1(hi.widenDealias)
              case _ => jsig1(elemtp)

        case RefOrAppliedType(sym, pre, args) =>
          if (sym == defn.PairClass && tupleArity(tp) > Definitions.MaxTupleArity)
            jsig1(defn.TupleXXLClass.typeRef)
          else if (isTypeParameterInSig(sym, sym0)) {
            assert(!sym.isAliasType || sym.info.isLambdaSub, "Unexpected alias type: " + sym)
            typeParamSig(sym.name.lastPart)
          }
          else if (defn.specialErasure.contains(sym))
            jsig1(defn.specialErasure(sym).typeRef)
          else if (sym == defn.UnitClass || sym == defn.BoxedUnitModule)
            jsig1(defn.BoxedUnitClass.typeRef)
          else if (sym == defn.NothingClass)
            builder.append("Lscala/runtime/Nothing$;")
          else if (sym == defn.NullClass)
            builder.append("Lscala/runtime/Null$;")
          else if (sym.isPrimitiveValueClass)
            if (!unboxedVCs) jsig1(defn.ObjectType)
            else if (sym == defn.UnitClass) jsig1(defn.BoxedUnitClass.typeRef)
            else builder.append(defn.typeTag(sym.info))
          else if (sym.isDerivedValueClass) {
            if (unboxedVCs) {
              val erasedUnderlying = fullErasure(tp)
              jsig(erasedUnderlying, toplevel = toplevel, unboxedVCs = true)
            } else classSig(sym, pre, args)
          }
          else if (defn.isSyntheticFunctionClass(sym)) {
            val erasedSym = defn.functionTypeErasure(sym).typeSymbol
            classSig(erasedSym, pre, if (erasedSym.typeParams.isEmpty) Nil else args)
          }
          else if sym.isClass then
            classSig(sym, pre, args)
          else
            jsig(erasure(tp), toplevel = toplevel, unboxedVCs = unboxedVCs)

        case ExprType(restpe) if toplevel =>
          builder.append("()")
          methodResultSig(restpe)

        case ExprType(restpe) =>
          jsig1(defn.FunctionType(0).appliedTo(restpe))

        case mtd: MethodOrPoly =>
          val (tparams, vparams, rte) = collectMethodParams(mtd)
          if (toplevel && !sym0.isConstructor) {
            if (sym0.is(Method)) {
              val (usedMethodTypeParamNames, usedClassTypeParams) = collectUsedTypeParams(vparams :+ rte, sym0)
              val methodTypeParamNames = tparams.map(tp => sanitizeName(tp.paramName.lastPart)).toSet
              // Only add class type parameters to shadowedClassTypeParamNames if they are:
              // 1. Referenced in the method signature, AND
              // 2. Shadowed by a method type parameter with the same name
              // This will trigger renaming of the method type parameter
              usedClassTypeParams.foreach { classTypeParam =>
                val classTypeParamName = sanitizeName(classTypeParam.name)
                if methodTypeParamNames.contains(classTypeParamName) then
                  shadowedClassTypeParamNames += classTypeParamName
              }
            }
            polyParamSig(tparams)
          }
          builder.append('(')
          for vparam <- vparams do jsig1(vparam)
          builder.append(')')
          methodResultSig(rte)

        case OrNull(tp1) if !tp1.derivesFrom(defn.AnyValClass) =>
          // Special case for nullable union types whose underlying type is not a value class.
          // For example, `T | Null` where `T` is a type parameter becomes `T` in the signature;
          // `Int | Null` still becomes `Object`.
          jsig1(tp1)

        case tp: AndType =>
          // Only intersections appearing as the upper-bound of a type parameter
          // can be preserved in generic signatures and those are already
          // handled by `boundsSig`, so here we fallback to picking a parent of
          // the intersection to determine its overall signature. We must pick a
          // parent whose erasure matches the erasure of the intersection
          // because javac relies on the generic signature to determine the
          // bytecode signature. Additionally, we prefer picking a type
          // parameter since that will likely lead to a more precise type.
          val parents = flattenedIntersection(tp)
          val (reprParents, _) = splitIntersection(parents)
          val repr =
            reprParents.find(_.typeSymbol.is(TypeParam)).getOrElse(reprParents.head)
          jsig(repr, toplevel = false, unboxedVCs = unboxedVCs)

        case ci: ClassInfo =>
          val tParams = tp.typeParams
          if (toplevel) polyParamSig(tParams)
          superSig(ci.typeSymbol, ci.parents)

        case AnnotatedType(atp, _) =>
          jsig(atp, toplevel, unboxedVCs)

        case hktl: HKTypeLambda =>
          jsig(hktl.finalResultType, toplevel, unboxedVCs)

        case _ =>
          val etp = erasure(tp)
          if (etp eq tp) throw new UnknownSig
          else jsig(etp, toplevel, unboxedVCs)
      }
    }
    val throwsArgs = sym0.annotations flatMap ThrownException.unapply
    if (needsJavaSig(info, throwsArgs))
      try {
        jsig(info, toplevel = true)
        throwsArgs.foreach { t =>
          builder.append('^')
          jsig(t, toplevel = true)
        }
        Some(builder.toString)
      }
      catch { case _: UnknownSig => None }
    else None
  }

  private class UnknownSig extends Exception

  /* Drop redundant types (ones which are implemented by some other parent) from the immediate parents.
   * This is important on Android because there is otherwise an interface explosion.
   */
  private def minimizeParents(cls: Symbol, parents: List[Type])(using Context): List[Type] = if (parents.isEmpty) parents else {
    // val requiredDirect: Symbol => Boolean = requiredDirectInterfaces.getOrElse(cls, Set.empty)
    var rest   = parents.tail
    var leaves = collection.mutable.ListBuffer.empty[Type] += parents.head
    while (rest.nonEmpty) {
      val candidate = rest.head
      val candidateSym = candidate.typeSymbol
      // val required = requiredDirect(candidateSym) || !leaves.exists(t => t.typeSymbol isSubClass candidateSym)
      val required = !leaves.exists(t => t.typeSymbol.isSubClass(candidateSym))
      if (required) {
        leaves = leaves filter { t =>
          val ts = t.typeSymbol
          !(ts.is(Trait) || ts.is(PureInterface)) || !candidateSym.isSubClass(ts)
          // requiredDirect(ts) || !ts.isTraitOrInterface || !candidateSym.isSubClass(ts)
        }
        leaves += candidate
      }
      rest = rest.tail
    }
    leaves.toList
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
    def unapply(tp: Type)(using Context): Option[(Symbol, Type, List[Type])] = tp match {
      case TypeParamRef(_, _) =>
        Some((tp.typeSymbol, tp, Nil))
      case TermParamRef(_, _) =>
        Some((tp.termSymbol, tp, Nil))
      case TypeRef(pre, _) if !tp.typeSymbol.isAliasType =>
        val sym = tp.typeSymbol
        Some((sym, pre, Nil))
      case AppliedType(pre, args) =>
        Some((pre.typeSymbol, pre, args))
      case _ =>
        None
    }
  }

  private def needsJavaSig(tp: Type, throwsArgs: List[Type])(using Context): Boolean = !ctx.settings.XnoGenericSig.value && {
      def needs(tp: Type) = (new NeedsSigCollector).apply(false, tp)
      needs(tp) || throwsArgs.exists(needs)
  }

  private class NeedsSigCollector(using Context) extends TypeAccumulator[Boolean] {
    override def apply(x: Boolean, tp: Type): Boolean =
      if (!x)
        tp.dealias match {
          case RefinedType(parent, refinedName, refinedInfo) =>
            val sym = parent.typeSymbol
            if (sym == defn.ArrayClass) foldOver(x, refinedInfo)
            else true
          case tref @ TypeRef(pre, name) =>
            val sym = tref.typeSymbol
            if (sym.is(TypeParam) || sym.typeParams.nonEmpty) true
            else if (sym.isClass) foldOver(x, rebindInnerClass(pre, sym)) // #2585
            else foldOver(x, pre)
          case PolyType(_, _) =>
            true
          case ClassInfo(_, _, parents, _, _) =>
            foldOver(tp.typeParams.nonEmpty, parents)
          case AnnotatedType(tpe, _) =>
            foldOver(x, tpe)
          case ExprType(tpe) =>
            true
          case tp =>
            foldOver(x, tp)
        }
      else x
  }

  private def collectMethodParams(mtd: MethodOrPoly)(using Context): (List[TypeParamInfo], List[Type], Type) =
    val tparams = ListBuffer.empty[TypeParamInfo]
    val vparams = ListBuffer.empty[Type]

    @tailrec def recur(tpe: Type): Type = tpe match
      case mtd: MethodType =>
        vparams ++= mtd.paramInfos.filterNot(_.hasAnnotation(defn.ErasedParamAnnot))
        recur(mtd.resType)
      case PolyType(tps, tpe) =>
        tparams ++= tps
        recur(tpe)
      case _ =>
        tpe
    end recur

    val rte = recur(mtd)
    (tparams.toList, vparams.toList, rte)
  end collectMethodParams

  /** Collect type parameters that are actually used in the given types. */
  private def collectUsedTypeParams(types: List[Type], initialSymbol: Symbol)(using Context): (Set[Name], Set[Symbol]) =
    assert(initialSymbol.is(Method))
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
    (usedMethodTypeParamNames.toSet, usedClassTypeParams.toSet)
  end collectUsedTypeParams
}
