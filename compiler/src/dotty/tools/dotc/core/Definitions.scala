package dotty.tools
package dotc
package core

import scala.annotation.{threadUnsafe => tu}
import Types._, Contexts._, Symbols._, SymDenotations._, StdNames._, Names._, Phases._
import Flags._, Scopes._, Decorators._, NameOps._, Periods._, NullOpsDecorator._
import unpickleScala2.Scala2Unpickler.ensureConstructor
import scala.collection.mutable
import collection.mutable
import Denotations.SingleDenotation
import util.SimpleIdentityMap
import typer.ImportInfo.RootRef

import scala.annotation.tailrec

object Definitions {

  /** The maximum number of elements in a tuple or product.
   *  This should be removed once we go to hlists.
   */
  val MaxTupleArity: Int = 22

  /** The maximum arity N of a function type that's implemented
   *  as a trait `scala.FunctionN`. Functions of higher arity are possible,
   *  but are mapped in erasure to functions taking a single parameter of type
   *  Object[].
   *  The limit 22 is chosen for Scala2x interop. It could be something
   *  else without affecting the set of programs that can be compiled.
   */
  val MaxImplementedFunctionArity: Int = MaxTupleArity
}

/** A class defining symbols and types of standard definitions
 *
 */
class Definitions {
  import Definitions._

  private var initCtx: Context = _
  private given currentContext[Dummy_so_its_a_def] as Context = initCtx

  private def newPermanentSymbol[N <: Name](owner: Symbol, name: N, flags: FlagSet, info: Type) =
    newSymbol(owner, name, flags | Permanent, info)

  private def newPermanentClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet, infoFn: ClassSymbol => Type) =
    newClassSymbol(owner, name, flags | Permanent | NoInits | Open, infoFn)

  private def enterCompleteClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet, parents: List[TypeRef], decls: Scope = newScope) =
    newCompleteClassSymbol(owner, name, flags | Permanent | NoInits | Open, parents, decls).entered

  private def enterTypeField(cls: ClassSymbol, name: TypeName, flags: FlagSet, scope: MutableScope) =
    scope.enter(newPermanentSymbol(cls, name, flags, TypeBounds.empty))

  private def enterTypeParam(cls: ClassSymbol, name: TypeName, flags: FlagSet, scope: MutableScope) =
    enterTypeField(cls, name, flags | ClassTypeParamCreationFlags, scope)

  private def enterSyntheticTypeParam(cls: ClassSymbol, paramFlags: FlagSet, scope: MutableScope, suffix: String = "T0") =
    enterTypeParam(cls, suffix.toTypeName, paramFlags, scope)

  // NOTE: Ideally we would write `parentConstrs: => Type*` but SIP-24 is only
  // implemented in Dotty and not in Scala 2.
  // See <http://docs.scala-lang.org/sips/pending/repeated-byname.html>.
  private def enterSpecialPolyClass(name: TypeName, paramFlags: FlagSet, parentConstrs: => Seq[Type]): ClassSymbol = {
    val completer = new LazyType {
      def complete(denot: SymDenotation)(using Context): Unit = {
        val cls = denot.asClass.classSymbol
        val paramDecls = newScope
        val typeParam = enterSyntheticTypeParam(cls, paramFlags, paramDecls)
        def instantiate(tpe: Type) =
          if (tpe.typeParams.nonEmpty) tpe.appliedTo(typeParam.typeRef)
          else tpe
        val parents = parentConstrs.toList map instantiate
        denot.info = ClassInfo(ScalaPackageClass.thisType, cls, parents, paramDecls)
      }
    }
    newPermanentClassSymbol(ScalaPackageClass, name, Artifact, completer).entered
  }

  /** The trait FunctionN, ContextFunctionN, ErasedFunctionN or ErasedContextFunction, for some N
   *  @param  name   The name of the trait to be created
   *
   *  FunctionN traits follow this template:
   *
   *      trait FunctionN[T0,...T{N-1}, R] extends Object {
   *        def apply($x0: T0, ..., $x{N_1}: T{N-1}): R
   *      }
   *
   *  That is, they follow the template given for Function2..Function22 in the
   *  standard library, but without `tupled` and `curried` methods and without
   *  a `toString`.
   *
   *  ContextFunctionN traits follow this template:
   *
   *      trait ContextFunctionN[T0,...,T{N-1}, R] extends Object {
   *        def apply(using $x0: T0, ..., $x{N_1}: T{N-1}): R
   *      }
   *
   *  ErasedFunctionN traits follow this template:
   *
   *      trait ErasedFunctionN[T0,...,T{N-1}, R] extends Object {
   *        def apply(erased $x0: T0, ..., $x{N_1}: T{N-1}): R
   *      }
   *
   *  ErasedContextFunctionN traits follow this template:
   *
   *      trait ErasedContextFunctionN[T0,...,T{N-1}, R] extends Object {
   *        def apply(using erased $x0: T0, ..., $x{N_1}: T{N-1}): R
   *      }
   *
   *  ErasedFunctionN and ErasedContextFunctionN erase to Function0.
   */
  def newFunctionNTrait(name: TypeName): ClassSymbol = {
    val completer = new LazyType {
      def complete(denot: SymDenotation)(using Context): Unit = {
        val cls = denot.asClass.classSymbol
        val decls = newScope
        val arity = name.functionArity
        val paramNamePrefix = tpnme.scala ++ str.NAME_JOIN ++ name ++ str.EXPAND_SEPARATOR
        val argParamRefs = List.tabulate(arity) { i =>
          enterTypeParam(cls, paramNamePrefix ++ "T" ++ (i + 1).toString, Contravariant, decls).typeRef
        }
        val resParamRef = enterTypeParam(cls, paramNamePrefix ++ "R", Covariant, decls).typeRef
        val methodType = MethodType.companion(
          isJava = false,
          isContextual = name.isContextFunction,
          isImplicit = false,
          isErased = name.isErasedFunction)
        decls.enter(newMethod(cls, nme.apply, methodType(argParamRefs, resParamRef), Deferred))
        denot.info =
          ClassInfo(ScalaPackageClass.thisType, cls, ObjectType :: Nil, decls)
      }
    }
    val flags0 = Trait | NoInits
    val flags = if (name.isContextFunction) flags0 | Final else flags0
    newPermanentClassSymbol(ScalaPackageClass, name, flags, completer)
  }

  private def newMethod(cls: ClassSymbol, name: TermName, info: Type, flags: FlagSet = EmptyFlags): TermSymbol =
    newPermanentSymbol(cls, name, flags | Method, info).asTerm

  private def enterMethod(cls: ClassSymbol, name: TermName, info: Type, flags: FlagSet = EmptyFlags): TermSymbol =
    newMethod(cls, name, info, flags).entered

  private def enterAliasType(name: TypeName, tpe: Type, flags: FlagSet = EmptyFlags): TypeSymbol = {
    val sym = newPermanentSymbol(ScalaPackageClass, name, flags, TypeAlias(tpe))
    ScalaPackageClass.currentPackageDecls.enter(sym)
    sym
  }

  private def enterBinaryAlias(name: TypeName, op: (Type, Type) => Type): TypeSymbol =
    enterAliasType(name,
      HKTypeLambda(TypeBounds.empty :: TypeBounds.empty :: Nil)(
      tl => op(tl.paramRefs(0), tl.paramRefs(1))))

  private def enterPolyMethod(cls: ClassSymbol, name: TermName, typeParamCount: Int,
                    resultTypeFn: PolyType => Type,
                    flags: FlagSet = EmptyFlags,
                    bounds: TypeBounds = TypeBounds.empty,
                    useCompleter: Boolean = false) = {
    val tparamNames = PolyType.syntheticParamNames(typeParamCount)
    val tparamInfos = tparamNames map (_ => bounds)
    def ptype = PolyType(tparamNames)(_ => tparamInfos, resultTypeFn)
    val info =
      if (useCompleter)
        new LazyType {
          def complete(denot: SymDenotation)(using Context): Unit =
            denot.info = ptype
        }
      else ptype
    enterMethod(cls, name, info, flags)
  }

  private def enterT1ParameterlessMethod(cls: ClassSymbol, name: TermName, resultTypeFn: PolyType => Type, flags: FlagSet) =
    enterPolyMethod(cls, name, 1, resultTypeFn, flags)

  private def mkArityArray(name: String, arity: Int, countFrom: Int): Array[TypeRef] = {
    val arr = new Array[TypeRef](arity + 1)
    for (i <- countFrom to arity) arr(i) = requiredClassRef(name + i)
    arr
  }

  private def completeClass(cls: ClassSymbol, ensureCtor: Boolean = true): ClassSymbol = {
    if (ensureCtor) ensureConstructor(cls, EmptyScope)
    if (cls.linkedClass.exists) cls.linkedClass.markAbsent()
    cls
  }

  @tu lazy val RootClass: ClassSymbol = newPackageSymbol(
    NoSymbol, nme.ROOT, (root, rootcls) => ctx.base.rootLoader(root)).moduleClass.asClass
  @tu lazy val RootPackage: TermSymbol = newSymbol(
    NoSymbol, nme.ROOTPKG, PackageCreationFlags, TypeRef(NoPrefix, RootClass))

  @tu lazy val EmptyPackageVal: TermSymbol = newPackageSymbol(
    RootClass, nme.EMPTY_PACKAGE, (emptypkg, emptycls) => ctx.base.rootLoader(emptypkg)).entered
  @tu lazy val EmptyPackageClass: ClassSymbol = EmptyPackageVal.moduleClass.asClass

  /** A package in which we can place all methods and types that are interpreted specially by the compiler */
  @tu lazy val OpsPackageVal: TermSymbol = newCompletePackageSymbol(RootClass, nme.OPS_PACKAGE).entered
  @tu lazy val OpsPackageClass: ClassSymbol = OpsPackageVal.moduleClass.asClass

  @tu lazy val ScalaPackageVal: TermSymbol = requiredPackage(nme.scala)
  @tu lazy val ScalaMathPackageVal: TermSymbol = requiredPackage("scala.math")
  @tu lazy val ScalaPackageClass: ClassSymbol = {
    val cls = ScalaPackageVal.moduleClass.asClass
    cls.info.decls.openForMutations.useSynthesizer(
      name =>
        if (name.isTypeName && name.isSyntheticFunction) newFunctionNTrait(name.asTypeName)
        else NoSymbol)
    cls
  }
  @tu lazy val ScalaPackageObject: Symbol = requiredModule("scala.package")
  @tu lazy val ScalaRuntimePackageVal: TermSymbol = requiredPackage("scala.runtime")
  @tu lazy val ScalaRuntimePackageClass: ClassSymbol = ScalaRuntimePackageVal.moduleClass.asClass
  @tu lazy val JavaPackageVal: TermSymbol = requiredPackage(nme.java)
  @tu lazy val JavaPackageClass: ClassSymbol = JavaPackageVal.moduleClass.asClass
  @tu lazy val JavaLangPackageVal: TermSymbol = requiredPackage(jnme.JavaLang)
  @tu lazy val JavaLangPackageClass: ClassSymbol = JavaLangPackageVal.moduleClass.asClass

  // fundamental modules
  @tu lazy val SysPackage : Symbol = requiredModule("scala.sys.package")
    @tu lazy val Sys_error: Symbol = SysPackage.moduleClass.requiredMethod(nme.error)

  @tu lazy val ScalaXmlPackageClass: Symbol = getPackageClassIfDefined("scala.xml")

  @tu lazy val CompiletimePackageObject: Symbol = requiredModule("scala.compiletime.package")
    @tu lazy val Compiletime_erasedValue  : Symbol = CompiletimePackageObject.requiredMethod("erasedValue")
    @tu lazy val Compiletime_error        : Symbol = CompiletimePackageObject.requiredMethod(nme.error)
    @tu lazy val Compiletime_requireConst: Symbol = CompiletimePackageObject.requiredMethod("requireConst")
    @tu lazy val Compiletime_constValue   : Symbol = CompiletimePackageObject.requiredMethod("constValue")
    @tu lazy val Compiletime_constValueOpt: Symbol = CompiletimePackageObject.requiredMethod("constValueOpt")
    @tu lazy val Compiletime_summonFrom   : Symbol = CompiletimePackageObject.requiredMethod("summonFrom")
  @tu lazy val CompiletimeTestingPackageObject: Symbol = requiredModule("scala.compiletime.testing.package")
    @tu lazy val CompiletimeTesting_typeChecks: Symbol = CompiletimeTestingPackageObject.requiredMethod("typeChecks")
    @tu lazy val CompiletimeTesting_typeCheckErrors: Symbol = CompiletimeTestingPackageObject.requiredMethod("typeCheckErrors")
    @tu lazy val CompiletimeTesting_ErrorClass: ClassSymbol = requiredClass("scala.compiletime.testing.Error")
    @tu lazy val CompiletimeTesting_Error: Symbol = requiredModule("scala.compiletime.testing.Error")
      @tu lazy val CompiletimeTesting_Error_apply = CompiletimeTesting_Error.requiredMethod(nme.apply)
    @tu lazy val CompiletimeTesting_ErrorKind: Symbol = requiredModule("scala.compiletime.testing.ErrorKind")
      @tu lazy val CompiletimeTesting_ErrorKind_Parser: Symbol = CompiletimeTesting_ErrorKind.requiredMethod("Parser")
      @tu lazy val CompiletimeTesting_ErrorKind_Typer: Symbol = CompiletimeTesting_ErrorKind.requiredMethod("Typer")
  @tu lazy val CompiletimeOpsPackageObject: Symbol = requiredModule("scala.compiletime.ops.package")
    @tu lazy val CompiletimeOpsPackageObjectAny: Symbol = requiredModule("scala.compiletime.ops.package.any")
    @tu lazy val CompiletimeOpsPackageObjectInt: Symbol = requiredModule("scala.compiletime.ops.package.int")
    @tu lazy val CompiletimeOpsPackageObjectString: Symbol = requiredModule("scala.compiletime.ops.package.string")
    @tu lazy val CompiletimeOpsPackageObjectBoolean: Symbol = requiredModule("scala.compiletime.ops.package.boolean")

  /** The `scalaShadowing` package is used to safely modify classes and
   *  objects in scala so that they can be used from dotty. They will
   *  be visible as members of the `scala` package, replacing any objects
   *  or classes with the same name. But their binary artifacts are
   *  in `scalaShadowing` so they don't clash with the same-named `scala`
   *  members at runtime.
   */
  @tu lazy val ScalaShadowingPackage: TermSymbol = requiredPackage(nme.scalaShadowing)

  /** Note: We cannot have same named methods defined in Object and Any (and AnyVal, for that matter)
   *  because after erasure the Any and AnyVal references get remapped to the Object methods
   *  which would result in a double binding assertion failure.
   * Instead we do the following:
   *
   *  - Have some methods exist only in Any, and remap them with the Erasure denotation
   *    transformer to be owned by Object.
   *  - Have other methods exist only in Object.
   * To achieve this, we synthesize all Any and Object methods; Object methods no longer get
   * loaded from a classfile.
   */
  @tu lazy val AnyClass: ClassSymbol = completeClass(enterCompleteClassSymbol(ScalaPackageClass, tpnme.Any, Abstract, Nil), ensureCtor = false)
  def AnyType: TypeRef = AnyClass.typeRef
  @tu lazy val AnyValClass: ClassSymbol = completeClass(enterCompleteClassSymbol(ScalaPackageClass, tpnme.AnyVal, Abstract, List(AnyClass.typeRef)))
  def AnyValType: TypeRef = AnyValClass.typeRef

    @tu lazy val Any_== : TermSymbol           = enterMethod(AnyClass, nme.EQ, methOfAny(BooleanType), Final)
    @tu lazy val Any_!= : TermSymbol          = enterMethod(AnyClass, nme.NE, methOfAny(BooleanType), Final)
    @tu lazy val Any_equals: TermSymbol       = enterMethod(AnyClass, nme.equals_, methOfAny(BooleanType))
    @tu lazy val Any_hashCode: TermSymbol     = enterMethod(AnyClass, nme.hashCode_, MethodType(Nil, IntType))
    @tu lazy val Any_toString: TermSymbol     = enterMethod(AnyClass, nme.toString_, MethodType(Nil, StringType))
    @tu lazy val Any_## : TermSymbol          = enterMethod(AnyClass, nme.HASHHASH, ExprType(IntType), Final)
    @tu lazy val Any_isInstanceOf: TermSymbol = enterT1ParameterlessMethod(AnyClass, nme.isInstanceOf_, _ => BooleanType, Final)
    @tu lazy val Any_asInstanceOf: TermSymbol = enterT1ParameterlessMethod(AnyClass, nme.asInstanceOf_, _.paramRefs(0), Final)
    @tu lazy val Any_typeTest: TermSymbol     = enterT1ParameterlessMethod(AnyClass, nme.isInstanceOfPM, _ => BooleanType, Final | Synthetic | Artifact)
    @tu lazy val Any_typeCast: TermSymbol     = enterT1ParameterlessMethod(AnyClass, nme.asInstanceOfPM, _.paramRefs(0), Final | Synthetic | Artifact | StableRealizable)
      // generated by pattern matcher and explicit nulls, eliminated by erasure

    /** def getClass[A >: this.type](): Class[? <: A] */
    @tu lazy val Any_getClass: TermSymbol =
        enterPolyMethod(
          AnyClass, nme.getClass_, 1,
          pt => MethodType(Nil, ClassClass.typeRef.appliedTo(TypeBounds.upper(pt.paramRefs(0)))),
          Final,
          bounds = TypeBounds.lower(AnyClass.thisType))

    def AnyMethods: List[TermSymbol] = List(Any_==, Any_!=, Any_equals, Any_hashCode,
      Any_toString, Any_##, Any_getClass, Any_isInstanceOf, Any_asInstanceOf, Any_typeTest, Any_typeCast)

  @tu lazy val ObjectClass: ClassSymbol = {
    val cls = requiredClass("java.lang.Object")
    assert(!cls.isCompleted, "race for completing java.lang.Object")
    cls.info = ClassInfo(cls.owner.thisType, cls, AnyClass.typeRef :: Nil, newScope)
    cls.setFlag(NoInits | JavaDefined)

    // The companion object doesn't really exist, so it needs to be marked as
    // absent. Here we need to set it before completing attempt to load Object's
    // classfile, which causes issue #1648.
    val companion = JavaLangPackageVal.info.decl(nme.Object).symbol
    companion.moduleClass.markAbsent()
    companion.markAbsent()

    completeClass(cls)
  }
  def ObjectType: TypeRef = ObjectClass.typeRef

  /** A type alias of Object used to represent any reference to Object in a Java
   *  signature, the secret sauce is that subtype checking treats it specially:
   *
   *    tp <:< FromJavaObject
   *
   *  is equivalent to:
   *
   *    tp <:< Any
   *
   *  This is useful to avoid usability problems when interacting with Java
   *  code where Object is the top type. This is safe because this type will
   *  only appear in signatures of Java definitions in positions where `Object`
   *  might appear, let's enumerate all possible cases this gives us:
   *
   *  1. At the top level:
   *
   *       // A.java
   *       void meth1(Object arg) {}
   *       <T> void meth2(T arg) {} // T implicitly extends Object
   *
   *       // B.scala
   *       meth1(1) // OK
   *       meth2(1) // OK
   *
   *     This is safe even though Int is not a subtype of Object, because Erasure
   *     will detect the mismatch and box the value type.
   *
   *  2. In a class type parameter:
   *
   *       // A.java
   *       void meth3(scala.List<Object> arg) {}
   *       <T> void meth4(scala.List<T> arg) {}
   *
   *       // B.scala
   *       meth3(List[Int](1)) // OK
   *       meth4(List[Int](1)) // OK
   *
   *     At erasure, type parameters are removed and value types are boxed.
   *
   *  3. As the type parameter of an array:
   *
   *       // A.java
   *       void meth5(Object[] arg) {}
   *       <T> void meth6(T[] arg) {}
   *
   *       // B.scala
   *       meth5(Array[Int](1)) // error: Array[Int] is not a subtype of Array[Object]
   *       meth6(Array[Int](1)) // error: Array[Int] is not a subtype of Array[T & Object]
   *
   *
   *     This is a bit more subtle: at erasure, Arrays keep their type parameter,
   *     and primitive Arrays are not subtypes of reference Arrays on the JVM,
   *     so we can't pass an Array of Int where a reference Array is expected.
   *     Array is invariant in Scala, so `meth5` is safe even if we use `FromJavaObject`,
   *     but generic Arrays are treated specially: we always add `& Object` (and here
   *     we mean the normal java.lang.Object type) to these types when they come from
   *     Java signatures (see `translateJavaArrayElementType`), this ensure that `meth6`
   *     is safe to use.
   *
   *  4. As the repeated argument of a varargs method:
   *
   *       // A.java
   *       void meth7(Object... args) {}
   *       <T> void meth8(T... args) {}
   *
   *       // B.scala
   *       meth7(1) // OK
   *       meth8(1) // OK
   *       val ai = Array[Int](1)
   *       meth7(ai: _*) // OK (will copy the array)
   *       meth8(ai: _*) // OK (will copy the array)
   *
   *     Java repeated arguments are erased to arrays, so it would be safe to treat
   *     them in the same way: add an `& Object` to the parameter type to disallow
   *     passing primitives, but that would be very inconvenient as it is common to
   *     want to pass a primitive to an Object repeated argument (e.g.
   *     `String.format("foo: %d", 1)`). So instead we type them _without_ adding the
   *     `& Object` and let `ElimRepeated` take care of doing any necessary adaptation
   *     (note that adapting a primitive array to a reference array requires
   *     copying the whole array, so this transformation only preserves semantics
   *     if the callee does not try to mutate the varargs array which is a reasonable
   *     assumption to make).
   *
   *
   *  This mechanism is similar to `ObjectTpeJavaRef` in Scala 2, except that we
   *  create a new symbol with its own name, this is needed because this type
   *  can show up in inferred types and therefore needs to be preserved when
   *  pickling so that unpickled trees pass `-Ycheck`.
   *
   *  Note that by default we pretty-print `FromJavaObject` as `Object` or simply omit it
   *  if it's the sole upper-bound of a type parameter, use `-Yprint-debug` to explicitly
   *  display it.
   */
  @tu lazy val FromJavaObjectSymbol: TypeSymbol =
    newPermanentSymbol(OpsPackageClass, tpnme.FromJavaObject, JavaDefined, TypeAlias(ObjectType)).entered
  def FromJavaObjectType: TypeRef = FromJavaObjectSymbol.typeRef

  @tu lazy val AnyRefAlias: TypeSymbol = enterAliasType(tpnme.AnyRef, ObjectType)
  def AnyRefType: TypeRef = AnyRefAlias.typeRef

    @tu lazy val Object_eq: TermSymbol = enterMethod(ObjectClass, nme.eq, methOfAnyRef(BooleanType), Final)
    @tu lazy val Object_ne: TermSymbol = enterMethod(ObjectClass, nme.ne, methOfAnyRef(BooleanType), Final)
    @tu lazy val Object_synchronized: TermSymbol = enterPolyMethod(ObjectClass, nme.synchronized_, 1,
        pt => MethodType(List(pt.paramRefs(0)), pt.paramRefs(0)), Final)
    @tu lazy val Object_clone: TermSymbol = enterMethod(ObjectClass, nme.clone_, MethodType(Nil, ObjectType), Protected)
    @tu lazy val Object_finalize: TermSymbol = enterMethod(ObjectClass, nme.finalize_, MethodType(Nil, UnitType), Protected)
    @tu lazy val Object_notify: TermSymbol = enterMethod(ObjectClass, nme.notify_, MethodType(Nil, UnitType), Final)
    @tu lazy val Object_notifyAll: TermSymbol = enterMethod(ObjectClass, nme.notifyAll_, MethodType(Nil, UnitType), Final)
    @tu lazy val Object_wait: TermSymbol = enterMethod(ObjectClass, nme.wait_, MethodType(Nil, UnitType), Final)
    @tu lazy val Object_waitL: TermSymbol = enterMethod(ObjectClass, nme.wait_, MethodType(LongType :: Nil, UnitType), Final)
    @tu lazy val Object_waitLI: TermSymbol = enterMethod(ObjectClass, nme.wait_, MethodType(LongType :: IntType :: Nil, UnitType), Final)

    def ObjectMethods: List[TermSymbol] = List(Object_eq, Object_ne, Object_synchronized, Object_clone,
        Object_finalize, Object_notify, Object_notifyAll, Object_wait, Object_waitL, Object_waitLI)

  /** Methods in Object and Any that do not have a side effect */
  @tu lazy val pureMethods: List[TermSymbol] = List(Any_==, Any_!=, Any_equals, Any_hashCode,
    Any_toString, Any_##, Any_getClass, Any_isInstanceOf, Any_typeTest, Object_eq, Object_ne)

  @tu lazy val AnyKindClass: ClassSymbol = {
    val cls = newCompleteClassSymbol(ScalaPackageClass, tpnme.AnyKind, AbstractFinal | Permanent, Nil)
    if (!ctx.settings.YnoKindPolymorphism.value)
      // Enable kind-polymorphism by exposing scala.AnyKind
      cls.entered
    cls
  }
  def AnyKindType: TypeRef = AnyKindClass.typeRef

  @tu lazy val andType: TypeSymbol = enterBinaryAlias(tpnme.AND, AndType(_, _))
  @tu lazy val orType: TypeSymbol = enterBinaryAlias(tpnme.OR, OrType(_, _, soft = false))

  /** Marker method to indicate an argument to a call-by-name parameter.
   *  Created by byNameClosures and elimByName, eliminated by Erasure,
   */
  @tu lazy val cbnArg: TermSymbol = enterPolyMethod(OpsPackageClass, nme.cbnArg, 1,
      pt => MethodType(List(FunctionOf(Nil, pt.paramRefs(0))), pt.paramRefs(0)))

  /** Method representing a throw */
  @tu lazy val throwMethod: TermSymbol = enterMethod(OpsPackageClass, nme.THROWkw,
      MethodType(List(ThrowableType), NothingType))

  @tu lazy val NothingClass: ClassSymbol = enterCompleteClassSymbol(
    ScalaPackageClass, tpnme.Nothing, AbstractFinal, List(AnyClass.typeRef))
  def NothingType: TypeRef = NothingClass.typeRef
  @tu lazy val NullClass: ClassSymbol = {
    val parent = if (ctx.explicitNulls) AnyType else ObjectType
    enterCompleteClassSymbol(ScalaPackageClass, tpnme.Null, AbstractFinal, parent :: Nil)
  }
  def NullType: TypeRef = NullClass.typeRef

  /** An alias for null values that originate in Java code.
   *  This type gets special treatment in the Typer. Specifically, `UncheckedNull` can be selected through:
   *  e.g.
   *  ```
   *  // x: String|Null
   *  x.length // error: `Null` has no `length` field
   *  // x2: String|UncheckedNull
   *  x2.length // allowed by the Typer, but unsound (might throw NPE)
   *  ```
   */
  lazy val UncheckedNullAlias: TypeSymbol = {
    assert(ctx.explicitNulls)
    enterAliasType(tpnme.UncheckedNull, NullType)
  }
  def UncheckedNullAliasType: TypeRef = UncheckedNullAlias.typeRef

  @tu lazy val ImplicitScrutineeTypeSym =
    newPermanentSymbol(ScalaPackageClass, tpnme.IMPLICITkw, EmptyFlags, TypeBounds.empty).entered
  def ImplicitScrutineeTypeRef: TypeRef = ImplicitScrutineeTypeSym.typeRef


  @tu lazy val ScalaPredefModule: Symbol = requiredModule("scala.Predef")
    @tu lazy val Predef_conforms : Symbol = ScalaPredefModule.requiredMethod(nme.conforms_)
    @tu lazy val Predef_classOf  : Symbol = ScalaPredefModule.requiredMethod(nme.classOf)
    @tu lazy val Predef_identity : Symbol = ScalaPredefModule.requiredMethod(nme.identity)
    @tu lazy val Predef_undefined: Symbol = ScalaPredefModule.requiredMethod(nme.???)

  @tu lazy val SubTypeClass: ClassSymbol = requiredClass("scala.<:<")
  @tu lazy val SubType_refl: Symbol = SubTypeClass.companionModule.requiredMethod(nme.refl)

  @tu lazy val DummyImplicitClass: ClassSymbol = requiredClass("scala.DummyImplicit")

  @tu lazy val ScalaRuntimeModule: Symbol = requiredModule("scala.runtime.ScalaRunTime")
    def runtimeMethodRef(name: PreName): TermRef = ScalaRuntimeModule.requiredMethodRef(name)
    def ScalaRuntime_drop: Symbol = runtimeMethodRef(nme.drop).symbol
    @tu lazy val ScalaRuntime__hashCode: Symbol = ScalaRuntimeModule.requiredMethod(nme._hashCode_)
    @tu lazy val ScalaRuntime_toArray: Symbol = ScalaRuntimeModule.requiredMethod(nme.toArray)
    @tu lazy val ScalaRuntime_toObjectArray: Symbol = ScalaRuntimeModule.requiredMethod(nme.toObjectArray)

  @tu lazy val BoxesRunTimeModule: Symbol = requiredModule("scala.runtime.BoxesRunTime")
    @tu lazy val BoxesRunTimeModule_externalEquals: Symbol = BoxesRunTimeModule.info.decl(nme.equals_).suchThat(toDenot(_).info.firstParamTypes.size == 2).symbol
  @tu lazy val ScalaStaticsModule: Symbol = requiredModule("scala.runtime.Statics")
    def staticsMethodRef(name: PreName): TermRef = ScalaStaticsModule.requiredMethodRef(name)
    def staticsMethod(name: PreName): TermSymbol = ScalaStaticsModule.requiredMethod(name)

  // Dotty deviation: we cannot use a @tu lazy val here because @tu lazy vals in dotty
  // will return "null" when called recursively, see #1856.
  def DottyPredefModule: Symbol = {
    if (myDottyPredefModule == null) {
      myDottyPredefModule = requiredModule("dotty.DottyPredef")
      assert(myDottyPredefModule != null)
    }
    myDottyPredefModule
  }
  private var myDottyPredefModule: Symbol = _

  @tu lazy val DottyArraysModule: Symbol = requiredModule("dotty.runtime.Arrays")
    def newGenericArrayMethod(using Context): TermSymbol = DottyArraysModule.requiredMethod("newGenericArray")
    def newArrayMethod(using Context): TermSymbol = DottyArraysModule.requiredMethod("newArray")

  def getWrapVarargsArrayModule: Symbol = ScalaRuntimeModule

  // The set of all wrap{X, Ref}Array methods, where X is a value type
  val WrapArrayMethods: PerRun[collection.Set[Symbol]] = new PerRun({
    val methodNames = ScalaValueTypes.map(ast.tpd.wrapArrayMethodName) `union` Set(nme.wrapRefArray)
    methodNames.map(getWrapVarargsArrayModule.requiredMethod(_))
  })

  @tu lazy val ListClass: Symbol       = requiredClass("scala.collection.immutable.List")
  @tu lazy val ListModule: Symbol      = requiredModule("scala.collection.immutable.List")
  @tu lazy val NilModule: Symbol       = requiredModule("scala.collection.immutable.Nil")
  @tu lazy val ConsClass: Symbol       = requiredClass("scala.collection.immutable.::")
  @tu lazy val SeqFactoryClass: Symbol = requiredClass("scala.collection.SeqFactory")

  @tu lazy val SingletonClass: ClassSymbol =
    // needed as a synthetic class because Scala 2.x refers to it in classfiles
    // but does not define it as an explicit class.
    enterCompleteClassSymbol(
      ScalaPackageClass, tpnme.Singleton, PureInterfaceCreationFlags | Final,
      List(AnyClass.typeRef), EmptyScope)
  @tu lazy val SingletonType: TypeRef = SingletonClass.typeRef

  @tu lazy val CollectionSeqType: TypeRef = requiredClassRef("scala.collection.Seq")
  @tu lazy val SeqType: TypeRef = requiredClassRef("scala.collection.immutable.Seq")
  def SeqClass(using Context): ClassSymbol = SeqType.symbol.asClass
    @tu lazy val Seq_apply        : Symbol = SeqClass.requiredMethod(nme.apply)
    @tu lazy val Seq_head         : Symbol = SeqClass.requiredMethod(nme.head)
    @tu lazy val Seq_drop         : Symbol = SeqClass.requiredMethod(nme.drop)
    @tu lazy val Seq_lengthCompare: Symbol = SeqClass.requiredMethod(nme.lengthCompare, List(IntType))
    @tu lazy val Seq_length       : Symbol = SeqClass.requiredMethod(nme.length)
    @tu lazy val Seq_toSeq        : Symbol = SeqClass.requiredMethod(nme.toSeq)

  @tu lazy val StringOps: Symbol = requiredClass("scala.collection.StringOps")
    @tu lazy val StringOps_format: Symbol  = StringOps.requiredMethod(nme.format)

  @tu lazy val ArrayType: TypeRef = requiredClassRef("scala.Array")
  def ArrayClass(using Context): ClassSymbol = ArrayType.symbol.asClass
    @tu lazy val Array_apply     : Symbol = ArrayClass.requiredMethod(nme.apply)
    @tu lazy val Array_update    : Symbol = ArrayClass.requiredMethod(nme.update)
    @tu lazy val Array_length    : Symbol = ArrayClass.requiredMethod(nme.length)
    @tu lazy val Array_clone     : Symbol = ArrayClass.requiredMethod(nme.clone_)
    @tu lazy val ArrayConstructor: Symbol = ArrayClass.requiredMethod(nme.CONSTRUCTOR)

  @tu lazy val ArrayModule: Symbol = requiredModule("scala.Array")

  @tu lazy val UnitType: TypeRef = valueTypeRef("scala.Unit", java.lang.Void.TYPE, UnitEnc, nme.specializedTypeNames.Void)
  def UnitClass(using Context): ClassSymbol = UnitType.symbol.asClass
  def UnitModuleClass(using Context): Symbol = UnitType.symbol.asClass.linkedClass
  @tu lazy val BooleanType: TypeRef = valueTypeRef("scala.Boolean", java.lang.Boolean.TYPE, BooleanEnc, nme.specializedTypeNames.Boolean)
  def BooleanClass(using Context): ClassSymbol = BooleanType.symbol.asClass
    @tu lazy val Boolean_!  : Symbol = BooleanClass.requiredMethod(nme.UNARY_!)
    @tu lazy val Boolean_&& : Symbol = BooleanClass.requiredMethod(nme.ZAND) // ### harmonize required... calls
    @tu lazy val Boolean_|| : Symbol = BooleanClass.requiredMethod(nme.ZOR)
    @tu lazy val Boolean_== : Symbol =
      BooleanClass.info.member(nme.EQ).suchThat(_.info.firstParamTypes match {
        case List(pt) => pt.isRef(BooleanClass)
        case _ => false
      }).symbol
    @tu lazy val Boolean_!= : Symbol =
      BooleanClass.info.member(nme.NE).suchThat(_.info.firstParamTypes match {
        case List(pt) => pt.isRef(BooleanClass)
        case _ => false
      }).symbol

  @tu lazy val ByteType: TypeRef = valueTypeRef("scala.Byte", java.lang.Byte.TYPE, ByteEnc, nme.specializedTypeNames.Byte)
  def ByteClass(using Context): ClassSymbol = ByteType.symbol.asClass
  @tu lazy val ShortType: TypeRef = valueTypeRef("scala.Short", java.lang.Short.TYPE, ShortEnc, nme.specializedTypeNames.Short)
  def ShortClass(using Context): ClassSymbol = ShortType.symbol.asClass
  @tu lazy val CharType: TypeRef = valueTypeRef("scala.Char", java.lang.Character.TYPE, CharEnc, nme.specializedTypeNames.Char)
  def CharClass(using Context): ClassSymbol = CharType.symbol.asClass
  @tu lazy val IntType: TypeRef = valueTypeRef("scala.Int", java.lang.Integer.TYPE, IntEnc, nme.specializedTypeNames.Int)
  def IntClass(using Context): ClassSymbol = IntType.symbol.asClass
    @tu lazy val Int_-  : Symbol = IntClass.requiredMethod(nme.MINUS, List(IntType))
    @tu lazy val Int_+  : Symbol = IntClass.requiredMethod(nme.PLUS, List(IntType))
    @tu lazy val Int_/  : Symbol = IntClass.requiredMethod(nme.DIV, List(IntType))
    @tu lazy val Int_*  : Symbol = IntClass.requiredMethod(nme.MUL, List(IntType))
    @tu lazy val Int_== : Symbol = IntClass.requiredMethod(nme.EQ, List(IntType))
    @tu lazy val Int_>= : Symbol = IntClass.requiredMethod(nme.GE, List(IntType))
    @tu lazy val Int_<= : Symbol = IntClass.requiredMethod(nme.LE, List(IntType))
  @tu lazy val LongType: TypeRef = valueTypeRef("scala.Long", java.lang.Long.TYPE, LongEnc, nme.specializedTypeNames.Long)
  def LongClass(using Context): ClassSymbol = LongType.symbol.asClass
    @tu lazy val Long_+ : Symbol = LongClass.requiredMethod(nme.PLUS, List(LongType))
    @tu lazy val Long_* : Symbol = LongClass.requiredMethod(nme.MUL, List(LongType))
    @tu lazy val Long_/ : Symbol = LongClass.requiredMethod(nme.DIV, List(LongType))

  @tu lazy val FloatType: TypeRef = valueTypeRef("scala.Float", java.lang.Float.TYPE, FloatEnc, nme.specializedTypeNames.Float)
  def FloatClass(using Context): ClassSymbol = FloatType.symbol.asClass
  @tu lazy val DoubleType: TypeRef = valueTypeRef("scala.Double", java.lang.Double.TYPE, DoubleEnc, nme.specializedTypeNames.Double)
  def DoubleClass(using Context): ClassSymbol = DoubleType.symbol.asClass

  @tu lazy val BoxedUnitClass: ClassSymbol = requiredClass("scala.runtime.BoxedUnit")
    def BoxedUnit_UNIT(using Context): TermSymbol = BoxedUnitClass.linkedClass.requiredValue("UNIT")
    def BoxedUnit_TYPE(using Context): TermSymbol = BoxedUnitClass.linkedClass.requiredValue("TYPE")

  @tu lazy val BoxedBooleanClass: ClassSymbol = requiredClass("java.lang.Boolean")
  @tu lazy val BoxedByteClass   : ClassSymbol = requiredClass("java.lang.Byte")
  @tu lazy val BoxedShortClass  : ClassSymbol = requiredClass("java.lang.Short")
  @tu lazy val BoxedCharClass   : ClassSymbol = requiredClass("java.lang.Character")
  @tu lazy val BoxedIntClass    : ClassSymbol = requiredClass("java.lang.Integer")
  @tu lazy val BoxedLongClass   : ClassSymbol = requiredClass("java.lang.Long")
  @tu lazy val BoxedFloatClass  : ClassSymbol = requiredClass("java.lang.Float")
  @tu lazy val BoxedDoubleClass : ClassSymbol = requiredClass("java.lang.Double")

  @tu lazy val BoxedBooleanModule: TermSymbol = requiredModule("java.lang.Boolean")
  @tu lazy val BoxedByteModule   : TermSymbol = requiredModule("java.lang.Byte")
  @tu lazy val BoxedShortModule  : TermSymbol = requiredModule("java.lang.Short")
  @tu lazy val BoxedCharModule   : TermSymbol = requiredModule("java.lang.Character")
  @tu lazy val BoxedIntModule    : TermSymbol = requiredModule("java.lang.Integer")
  @tu lazy val BoxedLongModule   : TermSymbol = requiredModule("java.lang.Long")
  @tu lazy val BoxedFloatModule  : TermSymbol = requiredModule("java.lang.Float")
  @tu lazy val BoxedDoubleModule : TermSymbol = requiredModule("java.lang.Double")
  @tu lazy val BoxedUnitModule   : TermSymbol = requiredModule("java.lang.Void")

  @tu lazy val ByNameParamClass2x: ClassSymbol = enterSpecialPolyClass(tpnme.BYNAME_PARAM_CLASS, Covariant, Seq(AnyType))

  @tu lazy val RepeatedParamClass: ClassSymbol = enterSpecialPolyClass(tpnme.REPEATED_PARAM_CLASS, Covariant, Seq(ObjectType, SeqType))

  // fundamental classes
  @tu lazy val StringClass: ClassSymbol = requiredClass("java.lang.String")
  def StringType: Type = StringClass.typeRef
  @tu lazy val StringModule: Symbol = StringClass.linkedClass
    @tu lazy val String_+ : TermSymbol = enterMethod(StringClass, nme.raw.PLUS, methOfAny(StringType), Final)
    @tu lazy val String_valueOf_Object: Symbol = StringModule.info.member(nme.valueOf).suchThat(_.info.firstParamTypes match {
      case List(pt) => pt.isAny || pt.isAnyRef
      case _ => false
    }).symbol

  @tu lazy val JavaCloneableClass: ClassSymbol        = requiredClass("java.lang.Cloneable")
  @tu lazy val NullPointerExceptionClass: ClassSymbol = requiredClass("java.lang.NullPointerException")
  @tu lazy val IndexOutOfBoundsException: ClassSymbol = requiredClass("java.lang.IndexOutOfBoundsException")
  @tu lazy val ClassClass: ClassSymbol                = requiredClass("java.lang.Class")
  @tu lazy val BoxedNumberClass: ClassSymbol          = requiredClass("java.lang.Number")
  @tu lazy val ClassCastExceptionClass: ClassSymbol   = requiredClass("java.lang.ClassCastException")
    @tu lazy val ClassCastExceptionClass_stringConstructor: TermSymbol  = ClassCastExceptionClass.info.member(nme.CONSTRUCTOR).suchThat(_.info.firstParamTypes match {
      case List(pt) =>
        val pt1 = if (ctx.explicitNulls) pt.stripNull() else pt
        pt1.isRef(StringClass)
      case _ => false
    }).symbol.asTerm
  @tu lazy val ArithmeticExceptionClass: ClassSymbol  = requiredClass("java.lang.ArithmeticException")
    @tu lazy val ArithmeticExceptionClass_stringConstructor: TermSymbol  = ArithmeticExceptionClass.info.member(nme.CONSTRUCTOR).suchThat(_.info.firstParamTypes match {
      case List(pt) =>
        val pt1 = if (ctx.explicitNulls) pt.stripNull() else pt
        pt1.isRef(StringClass)
      case _ => false
    }).symbol.asTerm

  @tu lazy val JavaSerializableClass: ClassSymbol     = requiredClass("java.io.Serializable")

  @tu lazy val ComparableClass: ClassSymbol           = requiredClass("java.lang.Comparable")

  @tu lazy val SystemClass: ClassSymbol               = requiredClass("java.lang.System")
  @tu lazy val SystemModule: Symbol              = SystemClass.linkedClass

  @tu lazy val NoSuchElementExceptionClass = requiredClass("java.util.NoSuchElementException")
  def NoSuchElementExceptionType = NoSuchElementExceptionClass.typeRef
  @tu lazy val IllegalArgumentExceptionClass = requiredClass("java.lang.IllegalArgumentException")
  def IllegalArgumentExceptionType = IllegalArgumentExceptionClass.typeRef

  // in scalac modified to have Any as parent

  @tu lazy val ThrowableType: TypeRef          = requiredClassRef("java.lang.Throwable")
  def ThrowableClass(using Context): ClassSymbol = ThrowableType.symbol.asClass
  @tu lazy val SerializableType: TypeRef       = JavaSerializableClass.typeRef
  def SerializableClass(using Context): ClassSymbol = SerializableType.symbol.asClass

  @tu lazy val JavaBigIntegerClass: ClassSymbol = requiredClass("java.math.BigInteger")
  @tu lazy val JavaBigDecimalClass: ClassSymbol = requiredClass("java.math.BigDecimal")
  @tu lazy val JavaCalendarClass: ClassSymbol = requiredClass("java.util.Calendar")
  @tu lazy val JavaDateClass: ClassSymbol = requiredClass("java.util.Date")
  @tu lazy val JavaFormattableClass: ClassSymbol = requiredClass("java.util.Formattable")

  @tu lazy val JavaEnumClass: ClassSymbol = {
    val cls = requiredClass("java.lang.Enum")
    // jl.Enum has a single constructor protected(name: String, ordinal: Int).
    // We remove the arguments from the primary constructor, and enter
    // a new constructor symbol with 2 arguments, so that both
    // `X extends jl.Enum[X]` and `X extends jl.Enum[X](name, ordinal)`
    // pass typer and go through jl.Enum-specific checks in RefChecks.
    cls.infoOrCompleter match {
      case completer: ClassfileLoader =>
        cls.info = new ClassfileLoader(completer.classfile) {
          override def complete(root: SymDenotation)(using Context): Unit = {
            super.complete(root)
            val constr = cls.primaryConstructor
            val noArgInfo = constr.info match {
              case info: PolyType =>
                info.resType match {
                  case meth: MethodType =>
                    info.derivedLambdaType(
                      resType = meth.derivedLambdaType(
                      paramNames = Nil, paramInfos = Nil))
                }
            }
            val argConstr = constr.copy().entered
            constr.info = noArgInfo
            constr.termRef.recomputeDenot()
          }
        }
        cls
    }
  }
  def JavaEnumType = JavaEnumClass.typeRef

  @tu lazy val StringBuilderClass: ClassSymbol = requiredClass("scala.collection.mutable.StringBuilder")
  @tu lazy val MatchErrorClass   : ClassSymbol = requiredClass("scala.MatchError")
  @tu lazy val ConversionClass   : ClassSymbol = requiredClass("scala.Conversion").typeRef.symbol.asClass

  @tu lazy val StringAddClass    : ClassSymbol = requiredClass("scala.runtime.StringAdd")
    @tu lazy val StringAdd_+ : Symbol = StringAddClass.requiredMethod(nme.raw.PLUS)

  @tu lazy val StringContextClass: ClassSymbol = requiredClass("scala.StringContext")
    @tu lazy val StringContext_s  : Symbol = StringContextClass.requiredMethod(nme.s)
    @tu lazy val StringContext_raw: Symbol = StringContextClass.requiredMethod(nme.raw_)
    @tu lazy val StringContext_f  : Symbol = StringContextClass.requiredMethod(nme.f)
    @tu lazy val StringContext_parts: Symbol = StringContextClass.requiredMethod(nme.parts)
  @tu lazy val StringContextModule: Symbol = StringContextClass.companionModule
    @tu lazy val StringContextModule_apply: Symbol = StringContextModule.requiredMethod(nme.apply)
    @tu lazy val StringContextModule_standardInterpolator: Symbol = StringContextModule.requiredMethod(nme.standardInterpolator)
    @tu lazy val StringContextModule_processEscapes: Symbol = StringContextModule.requiredMethod(nme.processEscapes)

  @tu lazy val PartialFunctionClass: ClassSymbol = requiredClass("scala.PartialFunction")
    @tu lazy val PartialFunction_isDefinedAt: Symbol = PartialFunctionClass.requiredMethod(nme.isDefinedAt)
    @tu lazy val PartialFunction_applyOrElse: Symbol = PartialFunctionClass.requiredMethod(nme.applyOrElse)

  @tu lazy val AbstractPartialFunctionClass: ClassSymbol = requiredClass("scala.runtime.AbstractPartialFunction")
  @tu lazy val FunctionXXLClass: ClassSymbol = requiredClass("scala.runtime.FunctionXXL")
  @tu lazy val ScalaSymbolClass: ClassSymbol = requiredClass("scala.Symbol")
  @tu lazy val DynamicClass: ClassSymbol = requiredClass("scala.Dynamic")
  @tu lazy val OptionClass: ClassSymbol = requiredClass("scala.Option")
  @tu lazy val SomeClass: ClassSymbol = requiredClass("scala.Some")
  @tu lazy val NoneModule: Symbol = requiredModule("scala.None")

  @tu lazy val EnumClass: ClassSymbol = requiredClass("scala.reflect.Enum")

  @tu lazy val EnumValueSerializationProxyClass: ClassSymbol = requiredClass("scala.runtime.EnumValueSerializationProxy")
    @tu lazy val EnumValueSerializationProxyConstructor: TermSymbol =
      EnumValueSerializationProxyClass.requiredMethod(nme.CONSTRUCTOR, List(ClassType(TypeBounds.empty), IntType))

  @tu lazy val ProductClass: ClassSymbol = requiredClass("scala.Product")
    @tu lazy val Product_canEqual          : Symbol = ProductClass.requiredMethod(nme.canEqual_)
    @tu lazy val Product_productArity      : Symbol = ProductClass.requiredMethod(nme.productArity)
    @tu lazy val Product_productElement    : Symbol = ProductClass.requiredMethod(nme.productElement)
    @tu lazy val Product_productElementName: Symbol = ProductClass.requiredMethod(nme.productElementName)
    @tu lazy val Product_productPrefix     : Symbol = ProductClass.requiredMethod(nme.productPrefix)

  @tu lazy val IteratorClass: ClassSymbol = requiredClass("scala.collection.Iterator")
  def IteratorModule(using Context): Symbol = IteratorClass.companionModule

  @tu lazy val ModuleSerializationProxyClass: ClassSymbol = requiredClass("scala.runtime.ModuleSerializationProxy")
    @tu lazy val ModuleSerializationProxyConstructor: TermSymbol =
      ModuleSerializationProxyClass.requiredMethod(nme.CONSTRUCTOR, List(ClassType(TypeBounds.empty)))

  @tu lazy val MirrorClass: ClassSymbol = requiredClass("scala.deriving.Mirror")
  @tu lazy val Mirror_ProductClass: ClassSymbol = requiredClass("scala.deriving.Mirror.Product")
    @tu lazy val Mirror_Product_fromProduct: Symbol = Mirror_ProductClass.requiredMethod(nme.fromProduct)
  @tu lazy val Mirror_SumClass: ClassSymbol = requiredClass("scala.deriving.Mirror.Sum")
  @tu lazy val Mirror_SingletonClass: ClassSymbol = requiredClass("scala.deriving.Mirror.Singleton")
  @tu lazy val Mirror_SingletonProxyClass: ClassSymbol = requiredClass("scala.deriving.Mirror.SingletonProxy")

  @tu lazy val LanguageModule: Symbol = requiredModule("scala.language")
  @tu lazy val LanguageExperimentalModule: Symbol = requiredModule("scala.language.experimental")
  @tu lazy val NonLocalReturnControlClass: ClassSymbol = requiredClass("scala.runtime.NonLocalReturnControl")
  @tu lazy val SelectableClass: ClassSymbol = requiredClass("scala.Selectable")

  @tu lazy val ReflectPackageClass: Symbol = requiredPackage("scala.reflect.package").moduleClass
  @tu lazy val ClassTagClass: ClassSymbol = requiredClass("scala.reflect.ClassTag")
  @tu lazy val ClassTagModule: Symbol = ClassTagClass.companionModule
    @tu lazy val ClassTagModule_apply: Symbol = ClassTagModule.requiredMethod(nme.apply)


  @tu lazy val QuotedExprClass: ClassSymbol = requiredClass("scala.quoted.Expr")
  @tu lazy val QuotedExprModule: Symbol = QuotedExprClass.companionModule

  @tu lazy val QuoteContextClass: ClassSymbol = requiredClass("scala.quoted.QuoteContext")
  @tu lazy val QuoteContextInternalClass: ClassSymbol = requiredClass("scala.internal.quoted.QuoteContextInternal")
    @tu lazy val QuoteContextInternal_unpickleExpr: Symbol = QuoteContextInternalClass.requiredMethod("unpickleExpr")
    @tu lazy val QuoteContextInternal_unpickleType: Symbol = QuoteContextInternalClass.requiredMethod("unpickleType")
    @tu lazy val QuoteContextInternal_ExprMatch: Symbol = QuoteContextInternalClass.requiredMethod("ExprMatch")
    @tu lazy val QuoteContextInternal_TypeMatch: Symbol = QuoteContextInternalClass.requiredMethod("TypeMatch")

  @tu lazy val LiftableModule: Symbol = requiredModule("scala.quoted.Liftable")
    @tu lazy val LiftableModule_BooleanLiftable: Symbol = LiftableModule.requiredMethod("BooleanLiftable")
    @tu lazy val LiftableModule_ByteLiftable: Symbol = LiftableModule.requiredMethod("ByteLiftable")
    @tu lazy val LiftableModule_ShortLiftable: Symbol = LiftableModule.requiredMethod("ShortLiftable")
    @tu lazy val LiftableModule_IntLiftable: Symbol = LiftableModule.requiredMethod("IntLiftable")
    @tu lazy val LiftableModule_LongLiftable: Symbol = LiftableModule.requiredMethod("LongLiftable")
    @tu lazy val LiftableModule_FloatLiftable: Symbol = LiftableModule.requiredMethod("FloatLiftable")
    @tu lazy val LiftableModule_DoubleLiftable: Symbol = LiftableModule.requiredMethod("DoubleLiftable")
    @tu lazy val LiftableModule_CharLiftable: Symbol = LiftableModule.requiredMethod("CharLiftable")
    @tu lazy val LiftableModule_StringLiftable: Symbol = LiftableModule.requiredMethod("StringLiftable")

  @tu lazy val InternalQuotedModule: Symbol = requiredModule("scala.internal.quoted.CompileTime")
    @tu lazy val InternalQuoted_exprQuote  : Symbol = InternalQuotedModule.requiredMethod("exprQuote")
    @tu lazy val InternalQuoted_exprSplice : Symbol = InternalQuotedModule.requiredMethod("exprSplice")
    @tu lazy val InternalQuoted_exprNestedSplice : Symbol = InternalQuotedModule.requiredMethod("exprNestedSplice")
    @tu lazy val InternalQuoted_QuoteTypeTagAnnot: ClassSymbol = InternalQuotedModule.requiredClass("quoteTypeTag")

  @tu lazy val InternalQuotedPatterns: Symbol = requiredModule("scala.internal.quoted.Patterns")
    @tu lazy val InternalQuotedPatterns_patternHole: Symbol = InternalQuotedPatterns.requiredMethod("patternHole")
    @tu lazy val InternalQuotedPatterns_patternHigherOrderHole: Symbol = InternalQuotedPatterns.requiredMethod("patternHigherOrderHole")
    @tu lazy val InternalQuotedPatterns_higherOrderHole: Symbol = InternalQuotedPatterns.requiredMethod("higherOrderHole")
    @tu lazy val InternalQuotedPatterns_patternTypeAnnot: ClassSymbol = InternalQuotedPatterns.requiredClass("patternType")
    @tu lazy val InternalQuotedPatterns_fromAboveAnnot: ClassSymbol = InternalQuotedPatterns.requiredClass("fromAbove")

  @tu lazy val QuotedTypeClass: ClassSymbol = requiredClass("scala.quoted.Type")
    @tu lazy val QuotedType_splice: Symbol = QuotedTypeClass.requiredType(tpnme.Underlying)

  @tu lazy val QuotedTypeModule: Symbol = QuotedTypeClass.companionModule
    @tu lazy val QuotedTypeModule_apply: Symbol = QuotedTypeModule.requiredMethod("apply")

  @tu lazy val TastyReflectionClass: ClassSymbol = requiredClass("scala.tasty.Reflection")

  @tu lazy val EqlClass: ClassSymbol = requiredClass("scala.Eql")
    def Eql_eqlAny(using Context): TermSymbol = EqlClass.companionModule.requiredMethod(nme.eqlAny)

  @tu lazy val TypeBoxClass: ClassSymbol = requiredClass("scala.internal.TypeBox")
    @tu lazy val TypeBox_CAP: TypeSymbol = TypeBoxClass.requiredType(tpnme.CAP)

  @tu lazy val MatchCaseClass: ClassSymbol = requiredClass("scala.internal.MatchCase")
  @tu lazy val NotClass: ClassSymbol = requiredClass("scala.util.Not")
    @tu lazy val Not_value: Symbol = NotClass.companionModule.requiredMethod(nme.value)

  @tu lazy val ValueOfClass: ClassSymbol = requiredClass("scala.ValueOf")

  @tu lazy val FromDigitsClass: ClassSymbol           = requiredClass("scala.util.FromDigits")
  @tu lazy val FromDigits_WithRadixClass: ClassSymbol = requiredClass("scala.util.FromDigits.WithRadix")
  @tu lazy val FromDigits_DecimalClass: ClassSymbol   = requiredClass("scala.util.FromDigits.Decimal")
  @tu lazy val FromDigits_FloatingClass: ClassSymbol  = requiredClass("scala.util.FromDigits.Floating")

  @tu lazy val XMLTopScopeModule: Symbol = requiredModule("scala.xml.TopScope")

  @tu lazy val CommandLineParserModule: Symbol = requiredModule("scala.util.CommandLineParser")
    @tu lazy val CLP_ParseError: ClassSymbol = CommandLineParserModule.requiredClass("ParseError").typeRef.symbol.asClass
    @tu lazy val CLP_parseArgument: Symbol = CommandLineParserModule.requiredMethod("parseArgument")
    @tu lazy val CLP_parseRemainingArguments: Symbol = CommandLineParserModule.requiredMethod("parseRemainingArguments")
    @tu lazy val CLP_showError: Symbol = CommandLineParserModule.requiredMethod("showError")

  @tu lazy val TupleTypeRef: TypeRef = requiredClassRef("scala.Tuple")
  def TupleClass(using Context): ClassSymbol = TupleTypeRef.symbol.asClass
    @tu lazy val Tuple_cons: Symbol = TupleClass.requiredMethod("*:")
  @tu lazy val EmptyTupleModule: Symbol = requiredModule("scala.EmptyTuple")
  @tu lazy val NonEmptyTupleTypeRef: TypeRef = requiredClassRef("scala.NonEmptyTuple")
  def NonEmptyTupleClass(using Context): ClassSymbol = NonEmptyTupleTypeRef.symbol.asClass
    lazy val NonEmptyTuple_tail: Symbol = NonEmptyTupleClass.requiredMethod("tail")
  @tu lazy val PairClass: ClassSymbol = requiredClass("scala.*:")

  @tu lazy val TupleXXLClass: ClassSymbol = requiredClass("scala.runtime.TupleXXL")
  def TupleXXLModule(using Context): Symbol = TupleXXLClass.companionModule

    def TupleXXL_fromIterator(using Context): Symbol = TupleXXLModule.requiredMethod("fromIterator")

  @tu lazy val RuntimeTupleModule: Symbol = requiredModule("scala.runtime.Tuple")
  @tu lazy val RuntimeTupleModuleClass: Symbol = RuntimeTupleModule.moduleClass
    lazy val RuntimeTuple_consIterator: Symbol = RuntimeTupleModule.requiredMethod("consIterator")
    lazy val RuntimeTuple_concatIterator: Symbol = RuntimeTupleModule.requiredMethod("concatIterator")
    lazy val RuntimeTuple_apply: Symbol = RuntimeTupleModule.requiredMethod("apply")
    lazy val RuntimeTuple_cons: Symbol = RuntimeTupleModule.requiredMethod("cons")
    lazy val RuntimeTuple_size: Symbol = RuntimeTupleModule.requiredMethod("size")
    lazy val RuntimeTuple_tail: Symbol = RuntimeTupleModule.requiredMethod("tail")
    lazy val RuntimeTuple_concat: Symbol = RuntimeTupleModule.requiredMethod("concat")
    lazy val RuntimeTuple_toArray: Symbol = RuntimeTupleModule.requiredMethod("toArray")
    lazy val RuntimeTuple_productToArray: Symbol = RuntimeTupleModule.requiredMethod("productToArray")
    lazy val RuntimeTuple_isInstanceOfTuple: Symbol = RuntimeTupleModule.requiredMethod("isInstanceOfTuple")
    lazy val RuntimeTuple_isInstanceOfEmptyTuple: Symbol = RuntimeTupleModule.requiredMethod("isInstanceOfEmptyTuple")
    lazy val RuntimeTuple_isInstanceOfNonEmptyTuple: Symbol = RuntimeTupleModule.requiredMethod("isInstanceOfNonEmptyTuple")

  @tu lazy val TupledFunctionTypeRef: TypeRef = requiredClassRef("scala.util.TupledFunction")
  def TupledFunctionClass(using Context): ClassSymbol = TupledFunctionTypeRef.symbol.asClass
  def RuntimeTupleFunctionsModule(using Context): Symbol = requiredModule("scala.runtime.TupledFunctions")

  // Annotation base classes
  @tu lazy val AnnotationClass: ClassSymbol = requiredClass("scala.annotation.Annotation")
  @tu lazy val ClassfileAnnotationClass: ClassSymbol = requiredClass("scala.annotation.ClassfileAnnotation")
  @tu lazy val StaticAnnotationClass: ClassSymbol = requiredClass("scala.annotation.StaticAnnotation")
  @tu lazy val RefiningAnnotationClass: ClassSymbol = requiredClass("scala.annotation.RefiningAnnotation")

  // Annotation classes
  @tu lazy val AnnotationDefaultAnnot: ClassSymbol = requiredClass("scala.annotation.internal.AnnotationDefault")
  @tu lazy val BodyAnnot: ClassSymbol = requiredClass("scala.annotation.internal.Body")
  @tu lazy val ChildAnnot: ClassSymbol = requiredClass("scala.annotation.internal.Child")
  @tu lazy val ContextResultCountAnnot: ClassSymbol = requiredClass("scala.annotation.internal.ContextResultCount")
  @tu lazy val DeprecatedAnnot: ClassSymbol = requiredClass("scala.deprecated")
  @tu lazy val ImplicitAmbiguousAnnot: ClassSymbol = requiredClass("scala.annotation.implicitAmbiguous")
  @tu lazy val ImplicitNotFoundAnnot: ClassSymbol = requiredClass("scala.annotation.implicitNotFound")
  @tu lazy val InlineParamAnnot: ClassSymbol = requiredClass("scala.annotation.internal.InlineParam")
  @tu lazy val InvariantBetweenAnnot: ClassSymbol = requiredClass("scala.annotation.internal.InvariantBetween")
  @tu lazy val MainAnnot: ClassSymbol = requiredClass("scala.main")
  @tu lazy val MigrationAnnot: ClassSymbol = requiredClass("scala.annotation.migration")
  @tu lazy val MixinAnnot: ClassSymbol = requiredClass("scala.annotation.mixin")
  @tu lazy val NativeAnnot: ClassSymbol = requiredClass("scala.native")
  @tu lazy val RepeatedAnnot: ClassSymbol = requiredClass("scala.annotation.internal.Repeated")
  @tu lazy val SourceFileAnnot: ClassSymbol = requiredClass("scala.annotation.internal.SourceFile")
  @tu lazy val ScalaSignatureAnnot: ClassSymbol = requiredClass("scala.reflect.ScalaSignature")
  @tu lazy val ScalaLongSignatureAnnot: ClassSymbol = requiredClass("scala.reflect.ScalaLongSignature")
  @tu lazy val ScalaStrictFPAnnot: ClassSymbol = requiredClass("scala.annotation.strictfp")
  @tu lazy val ScalaStaticAnnot: ClassSymbol = requiredClass("scala.annotation.static")
  @tu lazy val SerialVersionUIDAnnot: ClassSymbol = requiredClass("scala.SerialVersionUID")
  @tu lazy val TASTYSignatureAnnot: ClassSymbol = requiredClass("scala.annotation.internal.TASTYSignature")
  @tu lazy val TASTYLongSignatureAnnot: ClassSymbol = requiredClass("scala.annotation.internal.TASTYLongSignature")
  @tu lazy val TailrecAnnot: ClassSymbol = requiredClass("scala.annotation.tailrec")
  @tu lazy val ThreadUnsafeAnnot: ClassSymbol = requiredClass("scala.annotation.threadUnsafe")
  @tu lazy val ConstructorOnlyAnnot: ClassSymbol = requiredClass("scala.annotation.constructorOnly")
  @tu lazy val CompileTimeOnlyAnnot: ClassSymbol = requiredClass("scala.annotation.compileTimeOnly")
  @tu lazy val SwitchAnnot: ClassSymbol = requiredClass("scala.annotation.switch")
  @tu lazy val ThrowsAnnot: ClassSymbol = requiredClass("scala.throws")
  @tu lazy val TransientAnnot: ClassSymbol = requiredClass("scala.transient")
  @tu lazy val UncheckedAnnot: ClassSymbol = requiredClass("scala.unchecked")
  @tu lazy val UncheckedStableAnnot: ClassSymbol = requiredClass("scala.annotation.unchecked.uncheckedStable")
  @tu lazy val UncheckedVarianceAnnot: ClassSymbol = requiredClass("scala.annotation.unchecked.uncheckedVariance")
  @tu lazy val VolatileAnnot: ClassSymbol = requiredClass("scala.volatile")
  @tu lazy val FieldMetaAnnot: ClassSymbol = requiredClass("scala.annotation.meta.field")
  @tu lazy val GetterMetaAnnot: ClassSymbol = requiredClass("scala.annotation.meta.getter")
  @tu lazy val ParamMetaAnnot: ClassSymbol = requiredClass("scala.annotation.meta.param")
  @tu lazy val SetterMetaAnnot: ClassSymbol = requiredClass("scala.annotation.meta.setter")
  @tu lazy val ShowAsInfixAnnot: ClassSymbol = requiredClass("scala.annotation.showAsInfix")
  @tu lazy val FunctionalInterfaceAnnot: ClassSymbol = requiredClass("java.lang.FunctionalInterface")
  @tu lazy val InfixAnnot: ClassSymbol = requiredClass("scala.annotation.infix")
  @tu lazy val TargetNameAnnot: ClassSymbol = requiredClass("scala.annotation.targetName")
  @tu lazy val VarargsAnnot: ClassSymbol = requiredClass("scala.annotation.varargs")

  // A list of meta-annotations that are relevant for fields and accessors
  @tu lazy val FieldAccessorMetaAnnots: Set[Symbol] =
    Set(FieldMetaAnnot, GetterMetaAnnot, ParamMetaAnnot, SetterMetaAnnot)

  // A list of annotations that are commonly used to indicate that a field/method argument or return
  // type is not null. These annotations are used by the nullification logic in JavaNullInterop to
  // improve the precision of type nullification.
  // We don't require that any of these annotations be present in the class path, but we want to
  // create Symbols for the ones that are present, so they can be checked during nullification.
  @tu lazy val NotNullAnnots: List[ClassSymbol] = getClassesIfDefined(
    "javax.annotation.Nonnull" ::
    "javax.validation.constraints.NotNull" ::
    "androidx.annotation.NonNull" ::
    "android.support.annotation.NonNull" ::
    "android.annotation.NonNull" ::
    "com.android.annotations.NonNull" ::
    "org.eclipse.jdt.annotation.NonNull" ::
    "edu.umd.cs.findbugs.annotations.NonNull" ::
    "org.checkerframework.checker.nullness.qual.NonNull" ::
    "org.checkerframework.checker.nullness.compatqual.NonNullDecl" ::
    "org.jetbrains.annotations.NotNull" ::
    "org.springframework.lang.NonNull" ::
    "org.springframework.lang.NonNullApi" ::
    "org.springframework.lang.NonNullFields" ::
    "lombok.NonNull" ::
    "reactor.util.annotation.NonNull" ::
    "reactor.util.annotation.NonNullApi" ::
    "io.reactivex.annotations.NonNull" :: Nil)

  // convenient one-parameter method types
  def methOfAny(tp: Type): MethodType = MethodType(List(AnyType), tp)
  def methOfAnyVal(tp: Type): MethodType = MethodType(List(AnyValType), tp)
  def methOfAnyRef(tp: Type): MethodType = MethodType(List(ObjectType), tp)

  // Derived types

  def RepeatedParamType: TypeRef = RepeatedParamClass.typeRef

  def ClassType(arg: Type)(using Context): Type = {
    val ctype = ClassClass.typeRef
    if (ctx.phase.erasedTypes) ctype else ctype.appliedTo(arg)
  }

  /** The enumeration type, goven a value of the enumeration */
  def EnumType(sym: Symbol)(using Context): TypeRef =
    // given (in java): "class A { enum E { VAL1 } }"
    //  - sym: the symbol of the actual enumeration value (VAL1)
    //  - .owner: the ModuleClassSymbol of the enumeration (object E)
    //  - .linkedClass: the ClassSymbol of the enumeration (class E)
    sym.owner.linkedClass.typeRef

  object FunctionOf {
    def apply(args: List[Type], resultType: Type, isContextual: Boolean = false, isErased: Boolean = false)(using Context): Type =
      FunctionType(args.length, isContextual, isErased).appliedTo(args ::: resultType :: Nil)
    def unapply(ft: Type)(using Context): Option[(List[Type], Type, Boolean, Boolean)] = {
      val tsym = ft.typeSymbol
      if isFunctionClass(tsym) && ft.isRef(tsym) then
        val targs = ft.dealias.argInfos
        if (targs.isEmpty) None
        else Some(targs.init, targs.last, tsym.name.isContextFunction, tsym.name.isErasedFunction)
      else None
    }
  }

  object PartialFunctionOf {
    def apply(arg: Type, result: Type)(using Context): Type =
      PartialFunctionClass.typeRef.appliedTo(arg :: result :: Nil)
    def unapply(pft: Type)(using Context): Option[(Type, List[Type])] =
      if (pft.isRef(PartialFunctionClass)) {
        val targs = pft.dealias.argInfos
        if (targs.length == 2) Some((targs.head, targs.tail)) else None
      }
      else None
  }

  object ArrayOf {
    def apply(elem: Type)(using Context): Type =
      if (ctx.erasedTypes) JavaArrayType(elem)
      else ArrayType.appliedTo(elem :: Nil)
    def unapply(tp: Type)(using Context): Option[Type] = tp.dealias match {
      case AppliedType(at, arg :: Nil) if at.isRef(ArrayType.symbol) => Some(arg)
      case _ => None
    }
  }

  object MatchCase {
    def apply(pat: Type, body: Type)(using Context): Type =
      MatchCaseClass.typeRef.appliedTo(pat, body)
    def unapply(tp: Type)(using Context): Option[(Type, Type)] = tp match {
      case AppliedType(tycon, pat :: body :: Nil) if tycon.isRef(MatchCaseClass) =>
        Some((pat, body))
      case _ =>
        None
    }
    def isInstance(tp: Type)(using Context): Boolean = tp match {
      case AppliedType(tycon: TypeRef, _) =>
        tycon.name == tpnme.MatchCase && // necessary pre-filter to avoid forcing symbols
        tycon.isRef(MatchCaseClass)
      case _ => false
    }
  }

  /** An extractor for multi-dimensional arrays.
   *  Note that this will also extract the high bound if an
   *  element type is a wildcard. E.g.
   *
   *     Array[? <: Array[? <: Number]]
   *
   *  would match
   *
   *     MultiArrayOf(<Number>, 2)
   */
  object MultiArrayOf {
    def apply(elem: Type, ndims: Int)(using Context): Type =
      if (ndims == 0) elem else ArrayOf(apply(elem, ndims - 1))
    def unapply(tp: Type)(using Context): Option[(Type, Int)] = tp match {
      case ArrayOf(elemtp) =>
        def recur(elemtp: Type): Option[(Type, Int)] = elemtp.dealias match {
          case TypeBounds(lo, hi) => recur(hi)
          case MultiArrayOf(finalElemTp, n) => Some(finalElemTp, n + 1)
          case _ => Some(elemtp, 1)
        }
        recur(elemtp)
      case _ =>
        None
    }
  }

  final def isCompiletime_S(sym: Symbol)(using Context): Boolean =
    sym.name == tpnme.S && sym.owner == CompiletimePackageObject.moduleClass

  private val compiletimePackageAnyTypes: Set[Name] = Set(tpnme.Equals, tpnme.NotEquals)
  private val compiletimePackageIntTypes: Set[Name] = Set(
    tpnme.Plus, tpnme.Minus, tpnme.Times, tpnme.Div, tpnme.Mod,
    tpnme.Lt, tpnme.Gt, tpnme.Ge, tpnme.Le,
    tpnme.Abs, tpnme.Negate, tpnme.Min, tpnme.Max, tpnme.ToString,
    tpnme.Xor, tpnme.BitwiseAnd, tpnme.BitwiseOr, tpnme.ASR, tpnme.LSL, tpnme.LSR
  )
  private val compiletimePackageBooleanTypes: Set[Name] = Set(tpnme.Not, tpnme.Xor, tpnme.And, tpnme.Or)
  private val compiletimePackageStringTypes: Set[Name] = Set(tpnme.Plus)
  private val compiletimePackageOpTypes: Set[Name] =
    Set(tpnme.S)
    ++ compiletimePackageAnyTypes
    ++ compiletimePackageIntTypes
    ++ compiletimePackageBooleanTypes
    ++ compiletimePackageStringTypes

  final def isCompiletimeAppliedType(sym: Symbol)(using Context): Boolean =
    compiletimePackageOpTypes.contains(sym.name)
    && (
         sym.owner == CompiletimePackageObject.moduleClass && sym.name == tpnme.S
      || sym.owner == CompiletimeOpsPackageObjectAny.moduleClass && compiletimePackageAnyTypes.contains(sym.name)
      || sym.owner == CompiletimeOpsPackageObjectInt.moduleClass && compiletimePackageIntTypes.contains(sym.name)
      || sym.owner == CompiletimeOpsPackageObjectBoolean.moduleClass && compiletimePackageBooleanTypes.contains(sym.name)
      || sym.owner == CompiletimeOpsPackageObjectString.moduleClass && compiletimePackageStringTypes.contains(sym.name)
    )

  // ----- Symbol sets ---------------------------------------------------

  @tu lazy val AbstractFunctionType: Array[TypeRef] = mkArityArray("scala.runtime.AbstractFunction", MaxImplementedFunctionArity, 0)
  val AbstractFunctionClassPerRun: PerRun[Array[Symbol]] = new PerRun(AbstractFunctionType.map(_.symbol.asClass))
  def AbstractFunctionClass(n: Int)(using Context): Symbol = AbstractFunctionClassPerRun()(using ctx)(n)

  val LazyHolder: PerRun[Map[Symbol, Symbol]] = new PerRun({
    def holderImpl(holderType: String) = requiredClass("scala.runtime." + holderType)
    Map[Symbol, Symbol](
      IntClass     -> holderImpl("LazyInt"),
      LongClass    -> holderImpl("LazyLong"),
      BooleanClass -> holderImpl("LazyBoolean"),
      FloatClass   -> holderImpl("LazyFloat"),
      DoubleClass  -> holderImpl("LazyDouble"),
      ByteClass    -> holderImpl("LazyByte"),
      CharClass    -> holderImpl("LazyChar"),
      ShortClass   -> holderImpl("LazyShort")
    )
    .withDefaultValue(holderImpl("LazyRef"))
  })

  @tu lazy val TupleType: Array[TypeRef] = mkArityArray("scala.Tuple", MaxTupleArity, 1)

  private class FunType(prefix: String):
    private var classRefs: Array[TypeRef] = new Array(22)
    def apply(n: Int): TypeRef =
      while n >= classRefs.length do
        val classRefs1 = new Array[TypeRef](classRefs.length * 2)
        Array.copy(classRefs, 0, classRefs1, 0, classRefs.length)
        classRefs = classRefs1
      if classRefs(n) == null then
        classRefs(n) = requiredClassRef(prefix + n.toString)
      classRefs(n)

  private val erasedContextFunType = FunType("scala.ErasedContextFunction")
  private val contextFunType = FunType("scala.ContextFunction")
  private val erasedFunType = FunType("scala.ErasedFunction")
  private val funType = FunType("scala.Function")

  def FunctionClass(n: Int, isContextual: Boolean = false, isErased: Boolean = false)(using Context): Symbol =
    ( if isContextual && isErased then erasedContextFunType(n)
      else if isContextual then contextFunType(n)
      else if isErased then erasedFunType(n)
      else funType(n)
    ).symbol.asClass

  @tu lazy val Function0_apply: Symbol = FunctionClass(0).requiredMethod(nme.apply)

  def FunctionType(n: Int, isContextual: Boolean = false, isErased: Boolean = false)(using Context): TypeRef =
    FunctionClass(n, isContextual && !ctx.erasedTypes, isErased).typeRef

  lazy val PolyFunctionClass = requiredClass("scala.PolyFunction")
  def PolyFunctionType = PolyFunctionClass.typeRef

  /** If `cls` is a class in the scala package, its name, otherwise EmptyTypeName */
  def scalaClassName(cls: Symbol)(using Context): TypeName = cls.denot match
    case clsd: ClassDenotation if clsd.owner eq ScalaPackageClass =>
      clsd.name.asInstanceOf[TypeName]
    case _ =>
      EmptyTypeName

  /** If type `ref` refers to a class in the scala package, its name, otherwise EmptyTypeName */
  def scalaClassName(ref: Type)(using Context): TypeName = scalaClassName(ref.classSymbol)

  private def isVarArityClass(cls: Symbol, prefix: String) =
    cls.isClass
    && cls.owner.eq(ScalaPackageClass)
    && cls.name.testSimple(name =>
      name.startsWith(prefix)
      && name.length > prefix.length
      && digitsOnlyAfter(name, prefix.length))

  private def digitsOnlyAfter(name: SimpleName, idx: Int): Boolean =
    idx == name.length || name(idx).isDigit && digitsOnlyAfter(name, idx + 1)

  def isBottomClass(cls: Symbol): Boolean =
    if (ctx.explicitNulls && !ctx.phase.erasedTypes) cls == NothingClass
    else isBottomClassAfterErasure(cls)

  def isBottomClassAfterErasure(cls: Symbol): Boolean = cls == NothingClass || cls == NullClass

  /** Is a function class.
   *   - FunctionXXL
   *   - FunctionN for N >= 0
   *   - ContextFunctionN for N >= 0
   *   - ErasedFunctionN for N > 0
   *   - ErasedContextFunctionN for N > 0
   */
  def isFunctionClass(cls: Symbol): Boolean = scalaClassName(cls).isFunction

  /** Is an context function class.
   *   - ContextFunctionN for N >= 0
   *   - ErasedContextFunctionN for N > 0
   */
  def isContextFunctionClass(cls: Symbol): Boolean = scalaClassName(cls).isContextFunction

  /** Is an erased function class.
   *   - ErasedFunctionN for N > 0
   *   - ErasedContextFunctionN for N > 0
   */
  def isErasedFunctionClass(cls: Symbol): Boolean = scalaClassName(cls).isErasedFunction

  /** Is either FunctionXXL or  a class that will be erased to FunctionXXL
   *   - FunctionXXL
   *   - FunctionN for N >= 22
   *   - ContextFunctionN for N >= 22
   */
  def isXXLFunctionClass(cls: Symbol): Boolean = {
    val name = scalaClassName(cls)
    (name eq tpnme.FunctionXXL) || name.functionArity > MaxImplementedFunctionArity
  }

  /** Is a synthetic function class
   *    - FunctionN for N > 22
   *    - ContextFunctionN for N >= 0
   *    - ErasedFunctionN for N > 0
   *    - ErasedContextFunctionN for N > 0
   */
  def isSyntheticFunctionClass(cls: Symbol): Boolean = scalaClassName(cls).isSyntheticFunction

  def isAbstractFunctionClass(cls: Symbol): Boolean = isVarArityClass(cls, str.AbstractFunction)
  def isTupleClass(cls: Symbol): Boolean = isVarArityClass(cls, str.Tuple)
  def isProductClass(cls: Symbol): Boolean = isVarArityClass(cls, str.Product)

  def isBoxedUnitClass(cls: Symbol): Boolean =
    cls.isClass && (cls.owner eq ScalaRuntimePackageClass) && cls.name == tpnme.BoxedUnit

  def isScalaShadowingPackageClass(cls: Symbol): Boolean =
    cls.name == tpnme.scalaShadowing && cls.owner == RootClass

  /** Returns the erased class of the function class `cls`
   *    - FunctionN for N > 22 becomes FunctionXXL
   *    - FunctionN for 22 > N >= 0 remains as FunctionN
   *    - ContextFunctionN for N > 22 becomes FunctionXXL
   *    - ContextFunctionN for N <= 22 becomes FunctionN
   *    - ErasedFunctionN becomes Function0
   *    - ImplicitErasedFunctionN becomes Function0
   *    - anything else becomes a NoSymbol
   */
  def erasedFunctionClass(cls: Symbol): Symbol = {
    val arity = scalaClassName(cls).functionArity
    if (cls.name.isErasedFunction) FunctionClass(0)
    else if (arity > 22) FunctionXXLClass
    else if (arity >= 0) FunctionClass(arity)
    else NoSymbol
  }

  /** Returns the erased type of the function class `cls`
   *    - FunctionN for N > 22 becomes FunctionXXL
   *    - FunctionN for 22 > N >= 0 remains as FunctionN
   *    - ContextFunctionN for N > 22 becomes FunctionXXL
   *    - ContextFunctionN for N <= 22 becomes FunctionN
   *    - ErasedFunctionN becomes Function0
   *    - ImplicitErasedFunctionN becomes Function0
   *    - anything else becomes a NoType
   */
  def erasedFunctionType(cls: Symbol): Type = {
    val arity = scalaClassName(cls).functionArity
    if (cls.name.isErasedFunction) FunctionType(0)
    else if (arity > 22) FunctionXXLClass.typeRef
    else if (arity >= 0) FunctionType(arity)
    else NoType
  }

  val predefClassNames: Set[Name] =
    Set("Predef$", "DeprecatedPredef", "LowPriorityImplicits").map(_.toTypeName.unmangleClassName)

  /** Is `cls` the predef module class, or a class inherited by Predef? */
  def isPredefClass(cls: Symbol): Boolean =
    (cls.owner eq ScalaPackageClass) && predefClassNames.contains(cls.name)

  private val JavaImportFns: List[RootRef] = List(
    RootRef(() => JavaLangPackageVal.termRef)
  )

  private val ScalaImportFns: List[RootRef] =
    JavaImportFns :+
    RootRef(() => ScalaPackageVal.termRef)

  private val PredefImportFns: List[RootRef] = List(
    RootRef(() => ScalaPredefModule.termRef, isPredef=true),
    RootRef(() => DottyPredefModule.termRef)
  )

  @tu private lazy val JavaRootImportFns: List[RootRef] =
    if ctx.settings.YnoImports.value then Nil
    else JavaImportFns

  @tu private lazy val ScalaRootImportFns: List[RootRef] =
    if ctx.settings.YnoImports.value then Nil
    else if ctx.settings.YnoPredef.value then ScalaImportFns
    else ScalaImportFns ++ PredefImportFns

  @tu private lazy val JavaRootImportTypes: List[TermRef] = JavaRootImportFns.map(_.refFn())
  @tu private lazy val ScalaRootImportTypes: List[TermRef] = ScalaRootImportFns.map(_.refFn())
  @tu private lazy val JavaUnqualifiedOwnerTypes: Set[NamedType] = unqualifiedTypes(JavaRootImportTypes)
  @tu private lazy val ScalaUnqualifiedOwnerTypes: Set[NamedType] = unqualifiedTypes(ScalaRootImportTypes)

  /** Are we compiling a java source file? */
  private def isJavaContext(using Context): Boolean =
    val unit = ctx.compilationUnit
    unit != null && unit.isJava

  private def unqualifiedTypes(refs: List[TermRef]) =
    val types = refs.toSet[NamedType]
    types ++ types.map(_.symbol.moduleClass.typeRef)

  /** Lazy references to the root imports */
  def rootImportFns(using Context): List[RootRef] =
    if isJavaContext then JavaRootImportFns
    else ScalaRootImportFns

  /** Root types imported by default */
  def rootImportTypes(using Context): List[TermRef] =
    if isJavaContext then JavaRootImportTypes
    else ScalaRootImportTypes

  /** Modules whose members are in the default namespace and their module classes */
  def unqualifiedOwnerTypes(using Context): Set[NamedType] =
    if isJavaContext then JavaUnqualifiedOwnerTypes
    else ScalaUnqualifiedOwnerTypes

  /** Names of the root import symbols that can be hidden by other imports */
  @tu lazy val ShadowableImportNames: Set[TermName] = Set("Predef", "DottyPredef").map(_.toTermName)

  /** Class symbols for which no class exist at runtime */
  @tu lazy val NotRuntimeClasses: Set[Symbol] = Set(AnyClass, AnyValClass, NullClass, NothingClass)

  @tu lazy val SpecialClassTagClasses: Set[Symbol] = Set(UnitClass, AnyClass, AnyValClass)

  /** Classes that are known not to have an initializer irrespective of
   *  whether NoInits is set. Note: FunctionXXLClass is in this set
   *  because if it is compiled by Scala2, it does not get a NoInit flag.
   *  But since it is introduced only at erasure, there's no chance
   *  for augmentScala2Traits to do anything on a class that inherits it. So
   *  it also misses an implementation class, which means that the usual scheme
   *  of calling a superclass init in the implementation class of a Scala2
   *  trait gets screwed up. Therefore, it is mandatory that FunctionXXL
   *  is treated as a NoInit trait.
   */
  @tu lazy val NoInitClasses: Set[Symbol] = NotRuntimeClasses + FunctionXXLClass

  def isPolymorphicAfterErasure(sym: Symbol): Boolean =
     (sym eq Any_isInstanceOf) || (sym eq Any_asInstanceOf) || (sym eq Object_synchronized)

  def isTupleType(tp: Type)(using Context): Boolean = {
    val arity = tp.dealias.argInfos.length
    arity <= MaxTupleArity && TupleType(arity) != null && tp.isRef(TupleType(arity).symbol)
  }

  def tupleType(elems: List[Type]): Type = {
    val arity = elems.length
    if (0 < arity && arity <= MaxTupleArity && TupleType(arity) != null) TupleType(arity).appliedTo(elems)
    else TypeOps.nestedPairs(elems)
  }

  def tupleTypes(tp: Type, bound: Int = Int.MaxValue)(using Context): Option[List[Type]] = {
    @tailrec def rec(tp: Type, acc: List[Type], bound: Int): Option[List[Type]] = tp.normalized.dealias match {
      case _ if bound < 0 => Some(acc.reverse)
      case tp: AppliedType if defn.PairClass == tp.classSymbol => rec(tp.args(1), tp.args.head :: acc, bound - 1)
      case tp: AppliedType if defn.isTupleClass(tp.tycon.classSymbol) => Some(acc.reverse ::: tp.args)
      case tp: TermRef if tp.symbol == defn.EmptyTupleModule => Some(acc.reverse)
      case _ => None
    }
    rec(tp.stripTypeVar, Nil, bound)
  }

  def isProductSubType(tp: Type)(using Context): Boolean = tp.derivesFrom(ProductClass)

  /** Is `tp` (an alias) of either a scala.FunctionN or a scala.ContextFunctionN
   *  instance?
   */
  def isNonRefinedFunction(tp: Type)(using Context): Boolean =
    val arity = functionArity(tp)
    val sym = tp.dealias.typeSymbol

    arity >= 0
    && isFunctionClass(sym)
    && tp.isRef(
        FunctionType(arity, sym.name.isContextFunction, sym.name.isErasedFunction).typeSymbol,
        skipRefined = false)
  end isNonRefinedFunction

  /** Is `tp` a representation of a (possibly dependent) function type or an alias of such? */
  def isFunctionType(tp: Type)(using Context): Boolean =
    isNonRefinedFunction(tp.dropDependentRefinement)

  // Specialized type parameters defined for scala.Function{0,1,2}.
  @tu lazy val Function1SpecializedParamTypes: collection.Set[TypeRef] =
    Set(IntType, LongType, FloatType, DoubleType)
  @tu lazy val Function2SpecializedParamTypes: collection.Set[TypeRef] =
    Set(IntType, LongType, DoubleType)
  @tu lazy val Function0SpecializedReturnTypes: collection.Set[TypeRef] =
    ScalaNumericValueTypeList.toSet + UnitType + BooleanType
  @tu lazy val Function1SpecializedReturnTypes: collection.Set[TypeRef] =
    Set(UnitType, BooleanType, IntType, FloatType, LongType, DoubleType)
  @tu lazy val Function2SpecializedReturnTypes: collection.Set[TypeRef] =
    Function1SpecializedReturnTypes

  @tu lazy val Function1SpecializedParamClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(Function1SpecializedParamTypes.map(_.symbol))
  @tu lazy val Function2SpecializedParamClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(Function2SpecializedParamTypes.map(_.symbol))
  @tu lazy val Function0SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(Function0SpecializedReturnTypes.map(_.symbol))
  @tu lazy val Function1SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(Function1SpecializedReturnTypes.map(_.symbol))
  @tu lazy val Function2SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(Function2SpecializedReturnTypes.map(_.symbol))

  def isSpecializableFunction(cls: ClassSymbol, paramTypes: List[Type], retType: Type)(using Context): Boolean =
    paramTypes.length <= 2 && cls.derivesFrom(FunctionClass(paramTypes.length)) && (paramTypes match {
      case Nil =>
        Function0SpecializedReturnClasses().contains(retType.typeSymbol)
      case List(paramType0) =>
        Function1SpecializedParamClasses().contains(paramType0.typeSymbol) &&
        Function1SpecializedReturnClasses().contains(retType.typeSymbol)
      case List(paramType0, paramType1) =>
        Function2SpecializedParamClasses().contains(paramType0.typeSymbol) &&
        Function2SpecializedParamClasses().contains(paramType1.typeSymbol) &&
        Function2SpecializedReturnClasses().contains(retType.typeSymbol)
      case _ =>
        false
    })

  def functionArity(tp: Type)(using Context): Int = tp.dropDependentRefinement.dealias.argInfos.length - 1

  /** Return underlying context function type (i.e. instance of an ContextFunctionN class)
   *  or NoType if none exists. The following types are considered as underlying types:
   *   - the alias of an alias type
   *   - the instance or origin of a TypeVar (i.e. the result of a stripTypeVar)
   *   - the upper bound of a TypeParamRef in the current constraint
   */
  def asContextFunctionType(tp: Type)(using Context): Type =
    tp.stripTypeVar.dealias match
      case tp1: TypeParamRef if ctx.typerState.constraint.contains(tp1) =>
        asContextFunctionType(TypeComparer.bounds(tp1).hiBound)
      case tp1 =>
        if tp1.typeSymbol.name.isContextFunction && isFunctionType(tp1) then tp1
        else NoType

  /** Is `tp` an context function type? */
  def isContextFunctionType(tp: Type)(using Context): Boolean =
    asContextFunctionType(tp).exists

  /** An extractor for context function types `As ?=> B`, possibly with
   *  dependent refinements. Optionally returns a triple consisting of the argument
   *  types `As`, the result type `B` and a whether the type is an erased context function.
   */
  object ContextFunctionType:
    def unapply(tp: Type)(using Context): Option[(List[Type], Type, Boolean)] =
      if ctx.erasedTypes then
        atPhase(erasurePhase)(unapply(tp))
      else
        val tp1 = asContextFunctionType(tp)
        if tp1.exists then
          val args = tp1.dropDependentRefinement.argInfos
          Some((args.init, args.last, tp1.typeSymbol.name.isErasedFunction))
        else None

  def isErasedFunctionType(tp: Type)(using Context): Boolean =
    tp.dealias.typeSymbol.name.isErasedFunction && isFunctionType(tp)

  /** A whitelist of Scala-2 classes that are known to be pure */
  def isAssuredNoInits(sym: Symbol): Boolean =
    (sym `eq` SomeClass) || isTupleClass(sym)

  /** If `cls` is Tuple1..Tuple22, add the corresponding *: type as last parent to `parents` */
  def adjustForTuple(cls: ClassSymbol, tparams: List[TypeSymbol], parents: List[Type]): List[Type] = {
    def syntheticParent(tparams: List[TypeSymbol]): Type =
      if (tparams.isEmpty) TupleTypeRef
      else TypeOps.nestedPairs(tparams.map(_.typeRef))
    if (isTupleClass(cls)) parents :+ syntheticParent(tparams)
    else parents
  }

  /** If it is BoxedUnit, remove `java.io.Serializable` from `parents`. */
  def adjustForBoxedUnit(cls: ClassSymbol, parents: List[Type]): List[Type] =
    if (isBoxedUnitClass(cls)) parents.filter(_.typeSymbol != JavaSerializableClass)
    else parents

  private val HasProblematicGetClass: Set[Name] = Set(
    tpnme.AnyVal, tpnme.Byte, tpnme.Short, tpnme.Char, tpnme.Int, tpnme.Long, tpnme.Float, tpnme.Double,
    tpnme.Unit, tpnme.Boolean)

  /** When typing a primitive value class or AnyVal, we ignore the `getClass`
   *  member: it's supposed to be an override of the `getClass` defined on `Any`,
   *  but in dotty `Any#getClass` is polymorphic so it ends up being an overload.
   *  This is especially problematic because it means that when writing:
   *
   *    1.asInstanceOf[Int & AnyRef].getClass
   *
   *  the `getClass` that returns `Class[Int]` defined in Int can be selected,
   *  but this call is specified to return `classOf[Integer]`, see
   *  tests/run/t5568.scala.
   *
   *  FIXME: remove all the `getClass` methods defined in the standard library
   *  so we don't have to hot-patch it like this.
   */
  def hasProblematicGetClass(className: Name): Boolean =
    HasProblematicGetClass.contains(className)

  /** Is synthesized symbol with alphanumeric name allowed to be used as an infix operator? */
  def isInfix(sym: Symbol)(using Context): Boolean =
    (sym eq Object_eq) || (sym eq Object_ne)

  @tu lazy val assumedMixinTraits =
    Set(ComparableClass, ProductClass, SerializableClass,
      // add these for now, until we had a chance to retrofit 2.13 stdlib
      // we should do a more through sweep through it then.
      requiredClass("scala.collection.SortedOps"),
      requiredClass("scala.collection.StrictOptimizedSortedSetOps"),
      requiredClass("scala.collection.generic.DefaultSerializable"),
      requiredClass("scala.collection.generic.IsIterable"),
      requiredClass("scala.collection.generic.IsIterableOnce"),
      requiredClass("scala.collection.generic.IsMap"),
      requiredClass("scala.collection.generic.IsSeq"),
      requiredClass("scala.collection.generic.Subtractable"),
      requiredClass("scala.collection.immutable.StrictOptimizedSeqOps")
    )

  // ----- primitive value class machinery ------------------------------------------

  class PerRun[T](generate: Context ?=> T) {
    private var current: RunId = NoRunId
    private var cached: T = _
    def apply()(using Context): T = {
      if (current != ctx.runId) {
        cached = generate
        current = ctx.runId
      }
      cached
    }
  }

  @tu lazy val ScalaNumericValueTypeList: List[TypeRef] = List(
    ByteType, ShortType, CharType, IntType, LongType, FloatType, DoubleType)

  @tu private lazy val ScalaNumericValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypeList.toSet
  @tu private lazy val ScalaValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypes `union` Set(UnitType, BooleanType)

  val ScalaNumericValueClasses: PerRun[collection.Set[Symbol]] = new PerRun(ScalaNumericValueTypes.map(_.symbol))
  val ScalaValueClasses: PerRun[collection.Set[Symbol]]        = new PerRun(ScalaValueTypes.map(_.symbol))

  val ScalaBoxedClasses: PerRun[collection.Set[Symbol]] = new PerRun(
    Set(BoxedByteClass, BoxedShortClass, BoxedCharClass, BoxedIntClass, BoxedLongClass, BoxedFloatClass, BoxedDoubleClass, BoxedUnitClass, BoxedBooleanClass)
  )

  private val valueTypeEnc = mutable.Map[TypeName, PrimitiveClassEnc]()
  private val typeTags = mutable.Map[TypeName, Name]().withDefaultValue(nme.specializedTypeNames.Object)

//  private val unboxedTypeRef = mutable.Map[TypeName, TypeRef]()
//  private val javaTypeToValueTypeRef = mutable.Map[Class[?], TypeRef]()
//  private val valueTypeNamesToJavaType = mutable.Map[TypeName, Class[?]]()

  private def valueTypeRef(name: String, jtype: Class[?], enc: Int, tag: Name): TypeRef = {
    val vcls = requiredClassRef(name)
    valueTypeEnc(vcls.name) = enc
    typeTags(vcls.name) = tag
//    unboxedTypeRef(boxed.name) = vcls
//    javaTypeToValueTypeRef(jtype) = vcls
//    valueTypeNamesToJavaType(vcls.name) = jtype
    vcls
  }

  /** The type of the boxed class corresponding to primitive value type `tp`. */
  def boxedType(tp: Type)(using Context): TypeRef = {
    val cls = tp.classSymbol
    if (cls eq ByteClass)         BoxedByteClass
    else if (cls eq ShortClass)   BoxedShortClass
    else if (cls eq CharClass)    BoxedCharClass
    else if (cls eq IntClass)     BoxedIntClass
    else if (cls eq LongClass)    BoxedLongClass
    else if (cls eq FloatClass)   BoxedFloatClass
    else if (cls eq DoubleClass)  BoxedDoubleClass
    else if (cls eq UnitClass)    BoxedUnitClass
    else if (cls eq BooleanClass) BoxedBooleanClass
    else sys.error(s"Not a primitive value type: $tp")
  }.typeRef

  /** The JVM tag for `tp` if it's a primitive, `java.lang.Object` otherwise. */
  def typeTag(tp: Type)(using Context): Name = typeTags(scalaClassName(tp))

//  /** The `Class[?]` of a primitive value type name */
//  def valueTypeNameToJavaType(name: TypeName)(using Context): Option[Class[?]] =
//    valueTypeNamesToJavaType.get(if (name.firstPart eq nme.scala) name.lastPart.toTypeName else name)

  type PrimitiveClassEnc = Int

  val ByteEnc: Int = 2
  val ShortEnc: Int = ByteEnc * 3
  val CharEnc: Int = 5
  val IntEnc: Int = ShortEnc * CharEnc
  val LongEnc: Int = IntEnc * 7
  val FloatEnc: Int = LongEnc * 11
  val DoubleEnc: Int = FloatEnc * 13
  val BooleanEnc: Int = 17
  val UnitEnc: Int = 19

  def isValueSubType(tref1: TypeRef, tref2: TypeRef)(using Context): Boolean =
    valueTypeEnc(tref2.name) % valueTypeEnc(tref1.name) == 0
  def isValueSubClass(sym1: Symbol, sym2: Symbol): Boolean =
    valueTypeEnc(sym2.asClass.name) % valueTypeEnc(sym1.asClass.name) == 0

  @tu lazy val specialErasure: SimpleIdentityMap[Symbol, ClassSymbol] =
    SimpleIdentityMap.empty[Symbol]
      .updated(AnyClass, ObjectClass)
      .updated(AnyValClass, ObjectClass)
      .updated(SingletonClass, ObjectClass)
      .updated(TupleClass, ProductClass)
      .updated(NonEmptyTupleClass, ProductClass)

  // ----- Initialization ---------------------------------------------------

  /** Lists core classes that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  @tu lazy val syntheticScalaClasses: List[TypeSymbol] = {
    val synth = List(
      AnyClass,
      AnyRefAlias,
      AnyKindClass,
      andType,
      orType,
      RepeatedParamClass,
      ByNameParamClass2x,
      AnyValClass,
      NullClass,
      NothingClass,
      SingletonClass)

    if (ctx.explicitNulls) synth :+ UncheckedNullAlias else synth
  }

  @tu lazy val syntheticCoreClasses: List[Symbol] = syntheticScalaClasses ++ List(
    EmptyPackageVal,
    OpsPackageClass)

  /** Lists core methods that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  @tu lazy val syntheticCoreMethods: List[TermSymbol] =
    AnyMethods ++ ObjectMethods ++ List(String_+, throwMethod)

  @tu lazy val reservedScalaClassNames: Set[Name] = syntheticScalaClasses.map(_.name).toSet

  private var isInitialized = false

  def init()(using Context): Unit = {
    this.initCtx = ctx
    if (!isInitialized) {
      // Enter all symbols from the scalaShadowing package in the scala package
      for (m <- ScalaShadowingPackage.info.decls)
        ScalaPackageClass.enter(m)

      // force initialization of every symbol that is synthesized or hijacked by the compiler
      val forced = syntheticCoreClasses ++ syntheticCoreMethods ++ ScalaValueClasses() :+ JavaEnumClass

      isInitialized = true
    }
  }
}
