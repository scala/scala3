package dotty.tools
package dotc
package transform

import core.Annotations._
import core.Contexts._
import core.Phases._
import core.Definitions
import core.Flags._
import core.Names.Name
import core.Symbols._
import core.TypeApplications.TypeParamInfo
import core.TypeErasure.{erasedGlb, erasure, isGenericArrayElement}
import core.Types._
import core.classfile.ClassfileConstants
import SymUtils._
import TypeUtils._
import java.lang.StringBuilder

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
    val builder = new StringBuilder(64)
    val isTraitSignature = sym0.enclosingClass.is(Trait)

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

    def boxedSig(tp: Type): Unit = jsig(tp.widenDealias, primitiveOK = false)

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
      val (repr :: _, others) = splitIntersection(bounds)
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

    def paramSig(param: LambdaParam): Unit = {
      builder.append(sanitizeName(param.paramName))
      boundsSig(hiBounds(param.paramInfo.bounds))
    }

    def polyParamSig(tparams: List[LambdaParam]): Unit =
      if (tparams.nonEmpty) {
        builder.append('<')
        tparams.foreach(paramSig)
        builder.append('>')
      }

    def typeParamSig(name: Name): Unit = {
      builder.append(ClassfileConstants.TVAR_TAG)
      builder.append(sanitizeName(name))
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

    // This works as long as mangled names are always valid valid Java identifiers,
    // if we change our name encoding, we'll have to `throw new UnknownSig` here for
    // names which are not valid Java identifiers (see git history of this method).
    def sanitizeName(name: Name): String = name.mangledString

    // Anything which could conceivably be a module (i.e. isn't known to be
    // a type parameter or similar) must go through here or the signature is
    // likely to end up with Foo<T>.Empty where it needs Foo<T>.Empty$.
    def fullNameInSig(sym: Symbol): Unit = {
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
          case PolyType(_, res) =>
            builder.append('*') // scala/bug#7932
          case _: HKTypeLambda =>
            fullNameInSig(tp.typeSymbol)
            builder.append(';')
          case _ =>
            boxedSig(tp)
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
    def jsig(tp0: Type, toplevel: Boolean = false, primitiveOK: Boolean = true): Unit = {

      val tp = tp0.dealias
      tp match {

        case ref @ TypeParamRef(_: PolyType, _) =>
          typeParamSig(ref.paramName.lastPart)

        case defn.ArrayOf(elemtp) =>
          if (isGenericArrayElement(elemtp, isScala2 = false))
            jsig(defn.ObjectType)
          else
            builder.append(ClassfileConstants.ARRAY_TAG)
            elemtp match
              case TypeBounds(lo, hi) => jsig(hi.widenDealias)
              case _ => jsig(elemtp)

        case RefOrAppliedType(sym, pre, args) =>
          if (sym == defn.PairClass && tp.tupleArity > Definitions.MaxTupleArity)
            jsig(defn.TupleXXLClass.typeRef)
          else if (isTypeParameterInSig(sym, sym0)) {
            assert(!sym.isAliasType, "Unexpected alias type: " + sym)
            typeParamSig(sym.name.lastPart)
          }
          else if (defn.specialErasure.contains(sym))
            jsig(defn.specialErasure(sym).typeRef)
          else if (sym == defn.UnitClass || sym == defn.BoxedUnitModule)
            jsig(defn.BoxedUnitClass.typeRef)
          else if (sym == defn.NothingClass)
            builder.append("Lscala/runtime/Nothing$;")
          else if (sym == defn.NullClass)
            builder.append("Lscala/runtime/Null$;")
          else if (sym.isPrimitiveValueClass)
            if (!primitiveOK) jsig(defn.ObjectType)
            else if (sym == defn.UnitClass) jsig(defn.BoxedUnitClass.typeRef)
            else builder.append(defn.typeTag(sym.info))
          else if (ValueClasses.isDerivedValueClass(sym)) {
            val erasedUnderlying = core.TypeErasure.fullErasure(tp)
            if (erasedUnderlying.isPrimitiveValueType && !primitiveOK)
              classSig(sym, pre, args)
            else
              jsig(erasedUnderlying, toplevel, primitiveOK)
          }
          else if (defn.isSyntheticFunctionClass(sym)) {
            val erasedSym = defn.functionTypeErasure(sym).typeSymbol
            classSig(erasedSym, pre, if (erasedSym.typeParams.isEmpty) Nil else args)
          }
          else if sym.isClass then
            classSig(sym, pre, args)
          else
            jsig(erasure(tp), toplevel, primitiveOK)

        case ExprType(restpe) if toplevel =>
          builder.append("()")
          methodResultSig(restpe)

        case ExprType(restpe) =>
          jsig(defn.FunctionType(0).appliedTo(restpe))

        case PolyType(tparams, mtpe: MethodType) =>
          assert(tparams.nonEmpty)
          if (toplevel && !sym0.isConstructor) polyParamSig(tparams)
          jsig(mtpe)

        // Nullary polymorphic method
        case PolyType(tparams, restpe) =>
          assert(tparams.nonEmpty)
          if (toplevel) polyParamSig(tparams)
          builder.append("()")
          methodResultSig(restpe)

        case mtpe: MethodType =>
          // erased method parameters do not make it to the bytecode.
          def effectiveParamInfoss(t: Type)(using Context): List[List[Type]] = t match {
            case t: MethodType if t.isErasedMethod => effectiveParamInfoss(t.resType)
            case t: MethodType => t.paramInfos :: effectiveParamInfoss(t.resType)
            case _ => Nil
          }
          val params = effectiveParamInfoss(mtpe).flatten
          val restpe = mtpe.finalResultType
          builder.append('(')
          // TODO: Update once we support varargs
          params.foreach { tp =>
            jsig(tp)
          }
          builder.append(')')
          methodResultSig(restpe)

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
          jsig(repr, primitiveOK = primitiveOK)

        case ci: ClassInfo =>
          def polyParamSig(tparams: List[TypeParamInfo]): Unit =
            if (tparams.nonEmpty) {
              builder.append('<')
              tparams.foreach { tp =>
                builder.append(sanitizeName(tp.paramName.lastPart))
                boundsSig(hiBounds(tp.paramInfo.bounds))
              }
              builder.append('>')
            }
          val tParams = tp.typeParams
          if (toplevel) polyParamSig(tParams)
          superSig(ci.typeSymbol, ci.parents)

        case AnnotatedType(atp, _) =>
          jsig(atp, toplevel, primitiveOK)

        case hktl: HKTypeLambda =>
          jsig(hktl.finalResultType, toplevel, primitiveOK)

        case _ =>
          val etp = erasure(tp)
          if (etp eq tp) throw new UnknownSig
          else jsig(etp, toplevel, primitiveOK)
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
  // * higher-order type parameters
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

  private def needsJavaSig(tp: Type, throwsArgs: List[Type])(using Context): Boolean = !ctx.settings.YnoGenericSig.value && {
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
}
