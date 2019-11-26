package dotty.tools
package dotc
package core

import scala.annotation.{threadUnsafe => tu}
import Types._, Contexts._, Symbols._, SymDenotations._, StdNames._, Names._
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

  private implicit var ctx: Context = _

  private def newSymbol[N <: Name](owner: Symbol, name: N, flags: FlagSet, info: Type) =
    ctx.newSymbol(owner, name, flags | Permanent, info)

  private def newClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet, infoFn: ClassSymbol => Type) =
    ctx.newClassSymbol(owner, name, flags | Permanent | NoInits | Open, infoFn)

  private def enterCompleteClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet, parents: List[TypeRef], decls: Scope = newScope) =
    ctx.newCompleteClassSymbol(owner, name, flags | Permanent | NoInits | Open, parents, decls).entered

  private def enterTypeField(cls: ClassSymbol, name: TypeName, flags: FlagSet, scope: MutableScope) =
    scope.enter(newSymbol(cls, name, flags, TypeBounds.empty))

  private def enterTypeParam(cls: ClassSymbol, name: TypeName, flags: FlagSet, scope: MutableScope) =
    enterTypeField(cls, name, flags | ClassTypeParamCreationFlags, scope)

  private def enterSyntheticTypeParam(cls: ClassSymbol, paramFlags: FlagSet, scope: MutableScope, suffix: String = "T0") =
    enterTypeParam(cls, suffix.toTypeName, paramFlags, scope)

  // NOTE: Ideally we would write `parentConstrs: => Type*` but SIP-24 is only
  // implemented in Dotty and not in Scala 2.
  // See <http://docs.scala-lang.org/sips/pending/repeated-byname.html>.
  private def enterSpecialPolyClass(name: TypeName, paramFlags: FlagSet, parentConstrs: => Seq[Type]): ClassSymbol = {
    val completer = new LazyType {
      def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
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
    newClassSymbol(ScalaPackageClass, name, Artifact, completer).entered
  }

  /** The trait FunctionN, ImplicitFunctionN, ErasedFunctionN or ErasedImplicitFunction, for some N
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
   *  ImplicitFunctionN traits follow this template:
   *
   *      trait ImplicitFunctionN[T0,...,T{N-1}, R] extends Object {
   *        def apply(given $x0: T0, ..., $x{N_1}: T{N-1}): R
   *      }
   *
   *  ErasedFunctionN traits follow this template:
   *
   *      trait ErasedFunctionN[T0,...,T{N-1}, R] extends Object {
   *        def apply(erased $x0: T0, ..., $x{N_1}: T{N-1}): R
   *      }
   *
   *  ErasedImplicitFunctionN traits follow this template:
   *
   *      trait ErasedImplicitFunctionN[T0,...,T{N-1}, R] extends Object {
   *        def apply (given erased $x0: T0, ..., $x{N_1}: T{N-1}): R
   *      }
   *
   *  ErasedFunctionN and ErasedImplicitFunctionN erase to Function0.
   */
  def newFunctionNTrait(name: TypeName): ClassSymbol = {
    val completer = new LazyType {
      def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
        val cls = denot.asClass.classSymbol
        val decls = newScope
        val arity = name.functionArity
        val paramNamePrefix = tpnme.scala_ ++ str.NAME_JOIN ++ name ++ str.EXPAND_SEPARATOR
        val argParamRefs = List.tabulate(arity) { i =>
          enterTypeParam(cls, paramNamePrefix ++ "T" ++ (i + 1).toString, Contravariant, decls).typeRef
        }
        val resParamRef = enterTypeParam(cls, paramNamePrefix ++ "R", Covariant, decls).typeRef
        val methodType = MethodType.companion(
          isJava = false,
          isContextual = name.isImplicitFunction,
          isImplicit = false,
          isErased = name.isErasedFunction)
        decls.enter(newMethod(cls, nme.apply, methodType(argParamRefs, resParamRef), Deferred))
        denot.info =
          ClassInfo(ScalaPackageClass.thisType, cls, ObjectType :: Nil, decls)
      }
    }
    val flags0 = Trait | NoInits
    val flags = if (name.isImplicitFunction) flags0 | Final else flags0
    newClassSymbol(ScalaPackageClass, name, flags, completer)
  }

  private def newMethod(cls: ClassSymbol, name: TermName, info: Type, flags: FlagSet = EmptyFlags): TermSymbol =
    newSymbol(cls, name, flags | Method, info).asTerm

  private def enterMethod(cls: ClassSymbol, name: TermName, info: Type, flags: FlagSet = EmptyFlags): TermSymbol =
    newMethod(cls, name, info, flags).entered

  private def enterAliasType(name: TypeName, tpe: Type, flags: FlagSet = EmptyFlags): TypeSymbol = {
    val sym = newSymbol(ScalaPackageClass, name, flags, TypeAlias(tpe))
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
          def complete(denot: SymDenotation)(implicit ctx: Context): Unit =
            denot.info = ptype
        }
      else ptype
    enterMethod(cls, name, info, flags)
  }

  private def enterT1ParameterlessMethod(cls: ClassSymbol, name: TermName, resultTypeFn: PolyType => Type, flags: FlagSet) =
    enterPolyMethod(cls, name, 1, resultTypeFn, flags)

  private def mkArityArray(name: String, arity: Int, countFrom: Int): Array[TypeRef] = {
    val arr = new Array[TypeRef](arity + 1)
    for (i <- countFrom to arity) arr(i) = ctx.requiredClassRef(name + i)
    arr
  }

  private def completeClass(cls: ClassSymbol, ensureCtor: Boolean = true): ClassSymbol = {
    if (ensureCtor) ensureConstructor(cls, EmptyScope)
    if (cls.linkedClass.exists) cls.linkedClass.markAbsent()
    cls
  }

  @tu lazy val RootClass: ClassSymbol = ctx.newPackageSymbol(
    NoSymbol, nme.ROOT, (root, rootcls) => ctx.base.rootLoader(root)).moduleClass.asClass
  @tu lazy val RootPackage: TermSymbol = ctx.newSymbol(
    NoSymbol, nme.ROOTPKG, PackageCreationFlags, TypeRef(NoPrefix, RootClass))

  @tu lazy val EmptyPackageVal: TermSymbol = ctx.newPackageSymbol(
    RootClass, nme.EMPTY_PACKAGE, (emptypkg, emptycls) => ctx.base.rootLoader(emptypkg)).entered
  @tu lazy val EmptyPackageClass: ClassSymbol = EmptyPackageVal.moduleClass.asClass

  /** A package in which we can place all methods that are interpreted specially by the compiler */
  @tu lazy val OpsPackageVal: TermSymbol = ctx.newCompletePackageSymbol(RootClass, nme.OPS_PACKAGE).entered
  @tu lazy val OpsPackageClass: ClassSymbol = OpsPackageVal.moduleClass.asClass

  @tu lazy val ScalaPackageVal: TermSymbol = ctx.requiredPackage(nme.scala_)
  @tu lazy val ScalaMathPackageVal: TermSymbol = ctx.requiredPackage("scala.math")
  @tu lazy val ScalaPackageClass: ClassSymbol = {
    val cls = ScalaPackageVal.moduleClass.asClass
    cls.info.decls.openForMutations.useSynthesizer(
      name => ctx =>
        if (name.isTypeName && name.isSyntheticFunction) newFunctionNTrait(name.asTypeName)
        else NoSymbol)
    cls
  }
  @tu lazy val ScalaPackageObject: Symbol = ctx.requiredModule("scala.package")
  @tu lazy val JavaPackageVal: TermSymbol = ctx.requiredPackage(nme.java)
  @tu lazy val JavaLangPackageVal: TermSymbol = ctx.requiredPackage(jnme.JavaLang)

  // fundamental modules
  @tu lazy val SysPackage : Symbol = ctx.requiredModule("scala.sys.package")
    @tu lazy val Sys_error: Symbol = SysPackage.moduleClass.requiredMethod(nme.error)

  @tu lazy val ScalaXmlPackageClass: Symbol = ctx.getPackageClassIfDefined("scala.xml")

  @tu lazy val CompiletimePackageObject: Symbol = ctx.requiredModule("scala.compiletime.package")
    @tu lazy val Compiletime_error        : Symbol = CompiletimePackageObject.requiredMethod(nme.error)
    @tu lazy val Compiletime_constValue   : Symbol = CompiletimePackageObject.requiredMethod("constValue")
    @tu lazy val Compiletime_constValueOpt: Symbol = CompiletimePackageObject.requiredMethod("constValueOpt")
    @tu lazy val Compiletime_code         : Symbol = CompiletimePackageObject.requiredMethod("code")
    @tu lazy val Compiletime_summonFrom   : Symbol = CompiletimePackageObject.requiredMethod("summonFrom")
    @tu lazy val Compiletime_notNull      : Symbol = CompiletimePackageObject.requiredMethod("$notNull")
  @tu lazy val CompiletimeTestingPackageObject: Symbol = ctx.requiredModule("scala.compiletime.testing.package")
    @tu lazy val CompiletimeTesting_typeChecks: Symbol = CompiletimeTestingPackageObject.requiredMethod("typeChecks")
    @tu lazy val CompiletimeTesting_typeCheckErrors: Symbol = CompiletimeTestingPackageObject.requiredMethod("typeCheckErrors")
    @tu lazy val CompiletimeTesting_ErrorClass: ClassSymbol = ctx.requiredClass("scala.compiletime.testing.Error")
    @tu lazy val CompiletimeTesting_Error: Symbol = ctx.requiredModule("scala.compiletime.testing.Error")
      @tu lazy val CompiletimeTesting_Error_apply = CompiletimeTesting_Error.requiredMethod(nme.apply)
    @tu lazy val CompiletimeTesting_ErrorKind: Symbol = ctx.requiredModule("scala.compiletime.testing.ErrorKind")
      @tu lazy val CompiletimeTesting_ErrorKind_Parser: Symbol = CompiletimeTesting_ErrorKind.requiredMethod("Parser")
      @tu lazy val CompiletimeTesting_ErrorKind_Typer: Symbol = CompiletimeTesting_ErrorKind.requiredMethod("Typer")

  /** The `scalaShadowing` package is used to safely modify classes and
   *  objects in scala so that they can be used from dotty. They will
   *  be visible as members of the `scala` package, replacing any objects
   *  or classes with the same name. But their binary artifacts are
   *  in `scalaShadowing` so they don't clash with the same-named `scala`
   *  members at runtime.
   */
  @tu lazy val ScalaShadowingPackage: TermSymbol = ctx.requiredPackage(nme.scalaShadowing)

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
      // generated by pattern matcher, eliminated by erasure

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
    val cls = ctx.requiredClass("java.lang.Object")
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
    Any_toString, Any_##, Any_getClass, Any_isInstanceOf, Any_typeTest, Object_eq, Object_ne,
    Compiletime_notNull.asInstanceOf[TermSymbol])

  @tu lazy val AnyKindClass: ClassSymbol = {
    val cls = ctx.newCompleteClassSymbol(ScalaPackageClass, tpnme.AnyKind, AbstractFinal | Permanent, Nil)
    if (!ctx.settings.YnoKindPolymorphism.value)
      // Enable kind-polymorphism by exposing scala.AnyKind
      cls.entered
    cls
  }
  def AnyKindType: TypeRef = AnyKindClass.typeRef

  @tu lazy val andType: TypeSymbol = enterBinaryAlias(tpnme.AND, AndType(_, _))
  @tu lazy val orType: TypeSymbol = enterBinaryAlias(tpnme.OR, OrType(_, _))

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
  @tu lazy val RuntimeNothingModuleRef: TermRef = ctx.requiredModuleRef("scala.runtime.Nothing")
  @tu lazy val NullClass: ClassSymbol = {
    val parent = if (ctx.explicitNulls) AnyType else ObjectType
    enterCompleteClassSymbol(ScalaPackageClass, tpnme.Null, AbstractFinal, parent :: Nil)
  }
  def NullType: TypeRef = NullClass.typeRef
  @tu lazy val RuntimeNullModuleRef: TermRef = ctx.requiredModuleRef("scala.runtime.Null")

  /** An alias for null values that originate in Java code.
   *  This type gets special treatment in the Typer. Specifically, `JavaNull` can be selected through:
   *  e.g.
   *  ```
   *  // x: String|Null
   *  x.length // error: `Null` has no `length` field
   *  // x2: String|JavaNull
   *  x2.length // allowed by the Typer, but unsound (might throw NPE)
   *  ```
   */
  lazy val JavaNullAlias: TypeSymbol = {
    assert(ctx.explicitNulls)
    enterAliasType(tpnme.JavaNull, NullType)
  }
  def JavaNullAliasType: TypeRef = JavaNullAlias.typeRef

  @tu lazy val ImplicitScrutineeTypeSym =
    newSymbol(ScalaPackageClass, tpnme.IMPLICITkw, EmptyFlags, TypeBounds.empty).entered
  def ImplicitScrutineeTypeRef: TypeRef = ImplicitScrutineeTypeSym.typeRef


  @tu lazy val ScalaPredefModule: Symbol = ctx.requiredModule("scala.Predef")
    @tu lazy val Predef_conforms : Symbol = ScalaPredefModule.requiredMethod(nme.conforms_)
    @tu lazy val Predef_classOf  : Symbol = ScalaPredefModule.requiredMethod(nme.classOf)
    @tu lazy val Predef_identity : Symbol = ScalaPredefModule.requiredMethod(nme.identity)
    @tu lazy val Predef_undefined: Symbol = ScalaPredefModule.requiredMethod(nme.???)

  def SubTypeClass(implicit ctx: Context): ClassSymbol = ctx.requiredClass("scala.<:<")

  def DummyImplicitClass(implicit ctx: Context): ClassSymbol = ctx.requiredClass("scala.DummyImplicit")

  @tu lazy val ScalaRuntimeModule: Symbol = ctx.requiredModule("scala.runtime.ScalaRunTime")
    def runtimeMethodRef(name: PreName): TermRef = ScalaRuntimeModule.requiredMethodRef(name)
    def ScalaRuntime_drop: Symbol = runtimeMethodRef(nme.drop).symbol
    @tu lazy val ScalaRuntime__hashCode: Symbol = ScalaRuntimeModule.requiredMethod(nme._hashCode_)

  @tu lazy val BoxesRunTimeModule: Symbol = ctx.requiredModule("scala.runtime.BoxesRunTime")
  @tu lazy val ScalaStaticsModule: Symbol = ctx.requiredModule("scala.runtime.Statics")
    def staticsMethodRef(name: PreName): TermRef = ScalaStaticsModule.requiredMethodRef(name)
    def staticsMethod(name: PreName): TermSymbol = ScalaStaticsModule.requiredMethod(name)

  // Dotty deviation: we cannot use a @tu lazy val here because @tu lazy vals in dotty
  // will return "null" when called recursively, see #1856.
  def DottyPredefModule: Symbol = {
    if (myDottyPredefModule == null) {
      myDottyPredefModule = ctx.requiredModule("dotty.DottyPredef")
      assert(myDottyPredefModule != null)
    }
    myDottyPredefModule
  }
  private var myDottyPredefModule: Symbol = _

  @tu lazy val DottyArraysModule: Symbol = ctx.requiredModule("dotty.runtime.Arrays")
    def newGenericArrayMethod(implicit ctx: Context): TermSymbol = DottyArraysModule.requiredMethod("newGenericArray")
    def newArrayMethod(implicit ctx: Context): TermSymbol = DottyArraysModule.requiredMethod("newArray")

  def getWrapVarargsArrayModule: Symbol = ScalaRuntimeModule

  // The set of all wrap{X, Ref}Array methods, where X is a value type
  val WrapArrayMethods: PerRun[collection.Set[Symbol]] = new PerRun({ implicit ctx =>
    val methodNames = ScalaValueTypes.map(ast.tpd.wrapArrayMethodName) `union` Set(nme.wrapRefArray)
    methodNames.map(getWrapVarargsArrayModule.requiredMethod(_))
  })

  @tu lazy val ListModule: Symbol = ctx.requiredModule("scala.collection.immutable.List")
  @tu lazy val NilModule: Symbol = ctx.requiredModule("scala.collection.immutable.Nil")

  @tu lazy val SingletonClass: ClassSymbol =
    // needed as a synthetic class because Scala 2.x refers to it in classfiles
    // but does not define it as an explicit class.
    enterCompleteClassSymbol(
      ScalaPackageClass, tpnme.Singleton, PureInterfaceCreationFlags | Final,
      List(AnyClass.typeRef), EmptyScope)
  @tu lazy val SingletonType: TypeRef = SingletonClass.typeRef

  @tu lazy val CollectionSeqType: TypeRef = ctx.requiredClassRef("scala.collection.Seq")
  @tu lazy val SeqType: TypeRef = ctx.requiredClassRef("scala.collection.immutable.Seq")
  def SeqClass(given Context): ClassSymbol = SeqType.symbol.asClass
    @tu lazy val Seq_apply        : Symbol = SeqClass.requiredMethod(nme.apply)
    @tu lazy val Seq_head         : Symbol = SeqClass.requiredMethod(nme.head)
    @tu lazy val Seq_drop         : Symbol = SeqClass.requiredMethod(nme.drop)
    @tu lazy val Seq_lengthCompare: Symbol = SeqClass.requiredMethod(nme.lengthCompare, List(IntType))
    @tu lazy val Seq_length       : Symbol = SeqClass.requiredMethod(nme.length)
    @tu lazy val Seq_toSeq        : Symbol = SeqClass.requiredMethod(nme.toSeq)

  @tu lazy val ArrayType: TypeRef = ctx.requiredClassRef("scala.Array")
  def ArrayClass(given Context): ClassSymbol = ArrayType.symbol.asClass
    @tu lazy val Array_apply     : Symbol = ArrayClass.requiredMethod(nme.apply)
    @tu lazy val Array_update    : Symbol = ArrayClass.requiredMethod(nme.update)
    @tu lazy val Array_length    : Symbol = ArrayClass.requiredMethod(nme.length)
    @tu lazy val Array_clone     : Symbol = ArrayClass.requiredMethod(nme.clone_)
    @tu lazy val ArrayConstructor: Symbol = ArrayClass.requiredMethod(nme.CONSTRUCTOR)

  @tu lazy val ArrayModule: Symbol = ctx.requiredModule("scala.Array")

  @tu lazy val UnitType: TypeRef = valueTypeRef("scala.Unit", java.lang.Void.TYPE, UnitEnc, nme.specializedTypeNames.Void)
  def UnitClass(given Context): ClassSymbol = UnitType.symbol.asClass
  def UnitModuleClass(given Context): Symbol = UnitType.symbol.asClass.linkedClass
  @tu lazy val BooleanType: TypeRef = valueTypeRef("scala.Boolean", java.lang.Boolean.TYPE, BooleanEnc, nme.specializedTypeNames.Boolean)
  def BooleanClass(given Context): ClassSymbol = BooleanType.symbol.asClass
    @tu lazy val Boolean_!  : Symbol = BooleanClass.requiredMethod(nme.UNARY_!)
    @tu lazy val Boolean_&& : Symbol = BooleanClass.requiredMethod(nme.ZAND) // ### harmonize required... calls
    @tu lazy val Boolean_|| : Symbol = BooleanClass.requiredMethod(nme.ZOR)
    @tu lazy val Boolean_== : Symbol =
      BooleanClass.info.member(nme.EQ).suchThat(_.info.firstParamTypes match {
        case List(pt) => (pt isRef BooleanClass)
        case _ => false
      }).symbol
    @tu lazy val Boolean_!= : Symbol =
      BooleanClass.info.member(nme.NE).suchThat(_.info.firstParamTypes match {
        case List(pt) => (pt isRef BooleanClass)
        case _ => false
      }).symbol

  @tu lazy val ByteType: TypeRef = valueTypeRef("scala.Byte", java.lang.Byte.TYPE, ByteEnc, nme.specializedTypeNames.Byte)
  def ByteClass(given Context): ClassSymbol = ByteType.symbol.asClass
  @tu lazy val ShortType: TypeRef = valueTypeRef("scala.Short", java.lang.Short.TYPE, ShortEnc, nme.specializedTypeNames.Short)
  def ShortClass(given Context): ClassSymbol = ShortType.symbol.asClass
  @tu lazy val CharType: TypeRef = valueTypeRef("scala.Char", java.lang.Character.TYPE, CharEnc, nme.specializedTypeNames.Char)
  def CharClass(given Context): ClassSymbol = CharType.symbol.asClass
  @tu lazy val IntType: TypeRef = valueTypeRef("scala.Int", java.lang.Integer.TYPE, IntEnc, nme.specializedTypeNames.Int)
  def IntClass(given Context): ClassSymbol = IntType.symbol.asClass
    @tu lazy val Int_-  : Symbol = IntClass.requiredMethod(nme.MINUS, List(IntType))
    @tu lazy val Int_+  : Symbol = IntClass.requiredMethod(nme.PLUS, List(IntType))
    @tu lazy val Int_/  : Symbol = IntClass.requiredMethod(nme.DIV, List(IntType))
    @tu lazy val Int_*  : Symbol = IntClass.requiredMethod(nme.MUL, List(IntType))
    @tu lazy val Int_== : Symbol = IntClass.requiredMethod(nme.EQ, List(IntType))
    @tu lazy val Int_>= : Symbol = IntClass.requiredMethod(nme.GE, List(IntType))
    @tu lazy val Int_<= : Symbol = IntClass.requiredMethod(nme.LE, List(IntType))
  @tu lazy val LongType: TypeRef = valueTypeRef("scala.Long", java.lang.Long.TYPE, LongEnc, nme.specializedTypeNames.Long)
  def LongClass(given Context): ClassSymbol = LongType.symbol.asClass
    @tu lazy val Long_+ : Symbol = LongClass.requiredMethod(nme.PLUS, List(LongType))
    @tu lazy val Long_* : Symbol = LongClass.requiredMethod(nme.MUL, List(LongType))
    @tu lazy val Long_/ : Symbol = LongClass.requiredMethod(nme.DIV, List(LongType))

  @tu lazy val FloatType: TypeRef = valueTypeRef("scala.Float", java.lang.Float.TYPE, FloatEnc, nme.specializedTypeNames.Float)
  def FloatClass(given Context): ClassSymbol = FloatType.symbol.asClass
  @tu lazy val DoubleType: TypeRef = valueTypeRef("scala.Double", java.lang.Double.TYPE, DoubleEnc, nme.specializedTypeNames.Double)
  def DoubleClass(given Context): ClassSymbol = DoubleType.symbol.asClass

  @tu lazy val BoxedUnitClass: ClassSymbol = ctx.requiredClass("scala.runtime.BoxedUnit")
    def BoxedUnit_UNIT(given Context): TermSymbol = BoxedUnitClass.linkedClass.requiredValue("UNIT")
    def BoxedUnit_TYPE(given Context): TermSymbol = BoxedUnitClass.linkedClass.requiredValue("TYPE")

  @tu lazy val BoxedBooleanClass: ClassSymbol = ctx.requiredClass("java.lang.Boolean")
  @tu lazy val BoxedByteClass   : ClassSymbol = ctx.requiredClass("java.lang.Byte")
  @tu lazy val BoxedShortClass  : ClassSymbol = ctx.requiredClass("java.lang.Short")
  @tu lazy val BoxedCharClass   : ClassSymbol = ctx.requiredClass("java.lang.Character")
  @tu lazy val BoxedIntClass    : ClassSymbol = ctx.requiredClass("java.lang.Integer")
  @tu lazy val BoxedLongClass   : ClassSymbol = ctx.requiredClass("java.lang.Long")
  @tu lazy val BoxedFloatClass  : ClassSymbol = ctx.requiredClass("java.lang.Float")
  @tu lazy val BoxedDoubleClass : ClassSymbol = ctx.requiredClass("java.lang.Double")

  @tu lazy val BoxedBooleanModule: TermSymbol = ctx.requiredModule("java.lang.Boolean")
  @tu lazy val BoxedByteModule   : TermSymbol = ctx.requiredModule("java.lang.Byte")
  @tu lazy val BoxedShortModule  : TermSymbol = ctx.requiredModule("java.lang.Short")
  @tu lazy val BoxedCharModule   : TermSymbol = ctx.requiredModule("java.lang.Character")
  @tu lazy val BoxedIntModule    : TermSymbol = ctx.requiredModule("java.lang.Integer")
  @tu lazy val BoxedLongModule   : TermSymbol = ctx.requiredModule("java.lang.Long")
  @tu lazy val BoxedFloatModule  : TermSymbol = ctx.requiredModule("java.lang.Float")
  @tu lazy val BoxedDoubleModule : TermSymbol = ctx.requiredModule("java.lang.Double")
  @tu lazy val BoxedUnitModule   : TermSymbol = ctx.requiredModule("java.lang.Void")

  @tu lazy val ByNameParamClass2x: ClassSymbol = enterSpecialPolyClass(tpnme.BYNAME_PARAM_CLASS, Covariant, Seq(AnyType))

  @tu lazy val RepeatedParamClass: ClassSymbol = enterSpecialPolyClass(tpnme.REPEATED_PARAM_CLASS, Covariant, Seq(ObjectType, SeqType))

  // fundamental classes
  @tu lazy val StringClass: ClassSymbol = ctx.requiredClass("java.lang.String")
  def StringType: Type = StringClass.typeRef
  @tu lazy val StringModule: Symbol = StringClass.linkedClass
    @tu lazy val String_+ : TermSymbol = enterMethod(StringClass, nme.raw.PLUS, methOfAny(StringType), Final)
    @tu lazy val String_valueOf_Object: Symbol = StringModule.info.member(nme.valueOf).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef AnyClass) || (pt isRef ObjectClass)
      case _ => false
    }).symbol

  @tu lazy val JavaCloneableClass: ClassSymbol        = ctx.requiredClass("java.lang.Cloneable")
  @tu lazy val NullPointerExceptionClass: ClassSymbol = ctx.requiredClass("java.lang.NullPointerException")
  @tu lazy val IndexOutOfBoundsException: ClassSymbol = ctx.requiredClass("java.lang.IndexOutOfBoundsException")
  @tu lazy val ClassClass: ClassSymbol                = ctx.requiredClass("java.lang.Class")
  @tu lazy val BoxedNumberClass: ClassSymbol          = ctx.requiredClass("java.lang.Number")
  @tu lazy val ClassCastExceptionClass: ClassSymbol   = ctx.requiredClass("java.lang.ClassCastException")
    @tu lazy val ClassCastExceptionClass_stringConstructor: TermSymbol  = ClassCastExceptionClass.info.member(nme.CONSTRUCTOR).suchThat(_.info.firstParamTypes match {
      case List(pt) =>
        val pt1 = if (ctx.explicitNulls) pt.stripNull else pt
        pt1 isRef StringClass
      case _ => false
    }).symbol.asTerm
  @tu lazy val ArithmeticExceptionClass: ClassSymbol  = ctx.requiredClass("java.lang.ArithmeticException")
    @tu lazy val ArithmeticExceptionClass_stringConstructor: TermSymbol  = ArithmeticExceptionClass.info.member(nme.CONSTRUCTOR).suchThat(_.info.firstParamTypes match {
      case List(pt) =>
        val pt1 = if (ctx.explicitNulls) pt.stripNull else pt
        pt1 isRef StringClass
      case _ => false
    }).symbol.asTerm

  @tu lazy val JavaSerializableClass: ClassSymbol     = ctx.requiredClass("java.io.Serializable")

  @tu lazy val ComparableClass: ClassSymbol           = ctx.requiredClass("java.lang.Comparable")

  @tu lazy val SystemClass: ClassSymbol               = ctx.requiredClass("java.lang.System")
  @tu lazy val SystemModule: Symbol              = SystemClass.linkedClass

  @tu lazy val NoSuchElementExceptionClass = ctx.requiredClass("java.util.NoSuchElementException")
  def NoSuchElementExceptionType = NoSuchElementExceptionClass.typeRef
  @tu lazy val IllegalArgumentExceptionClass = ctx.requiredClass("java.lang.IllegalArgumentException")
  def IllegalArgumentExceptionType = IllegalArgumentExceptionClass.typeRef

  // in scalac modified to have Any as parent

  @tu lazy val ThrowableType: TypeRef          = ctx.requiredClassRef("java.lang.Throwable")
  def ThrowableClass(given Context): ClassSymbol = ThrowableType.symbol.asClass
  @tu lazy val SerializableType: TypeRef       = JavaSerializableClass.typeRef
  def SerializableClass(given Context): ClassSymbol = SerializableType.symbol.asClass

   @tu lazy val JavaEnumClass: ClassSymbol = {
    val cls = ctx.requiredClass("java.lang.Enum")
    cls.infoOrCompleter match {
      case completer: ClassfileLoader =>
        cls.info = new ClassfileLoader(completer.classfile) {
          override def complete(root: SymDenotation)(implicit ctx: Context): Unit = {
            super.complete(root)
            val constr = cls.primaryConstructor
            val newInfo = constr.info match {
              case info: PolyType =>
                info.resType match {
                  case meth: MethodType =>
                    info.derivedLambdaType(
                      resType = meth.derivedLambdaType(
                      paramNames = Nil, paramInfos = Nil))
                }
            }
            constr.info = newInfo
            constr.termRef.recomputeDenot()
          }
        }
        cls
    }
  }
  def JavaEnumType = JavaEnumClass.typeRef

  @tu lazy val StringBuilderClass: ClassSymbol = ctx.requiredClass("scala.collection.mutable.StringBuilder")
  @tu lazy val MatchErrorClass   : ClassSymbol = ctx.requiredClass("scala.MatchError")
  @tu lazy val ConversionClass   : ClassSymbol = ctx.requiredClass("scala.Conversion").typeRef.symbol.asClass

  @tu lazy val StringAddClass    : ClassSymbol = ctx.requiredClass("scala.runtime.StringAdd")
    @tu lazy val StringAdd_+ : Symbol = StringAddClass.requiredMethod(nme.raw.PLUS)

  @tu lazy val StringContextClass: ClassSymbol = ctx.requiredClass("scala.StringContext")
    @tu lazy val StringContext_s  : Symbol = StringContextClass.requiredMethod(nme.s)
    @tu lazy val StringContext_raw: Symbol = StringContextClass.requiredMethod(nme.raw_)
    @tu lazy val StringContext_f  : Symbol = StringContextClass.requiredMethod(nme.f)
    @tu lazy val StringContext_parts: Symbol = StringContextClass.requiredMethod(nme.parts)
  @tu lazy val StringContextModule: Symbol = StringContextClass.companionModule
    @tu lazy val StringContextModule_apply: Symbol = StringContextModule.requiredMethod(nme.apply)
    @tu lazy val StringContextModule_standardInterpolator: Symbol = StringContextModule.requiredMethod(nme.standardInterpolator)
    @tu lazy val StringContextModule_processEscapes: Symbol = StringContextModule.requiredMethod(nme.processEscapes)

  @tu lazy val InternalStringContextMacroModule: Symbol = ctx.requiredModule("dotty.internal.StringContextMacro")
    @tu lazy val InternalStringContextMacroModule_f: Symbol = InternalStringContextMacroModule.requiredMethod(nme.f)

  @tu lazy val PartialFunctionClass: ClassSymbol = ctx.requiredClass("scala.PartialFunction")
    @tu lazy val PartialFunction_isDefinedAt: Symbol = PartialFunctionClass.requiredMethod(nme.isDefinedAt)
    @tu lazy val PartialFunction_applyOrElse: Symbol = PartialFunctionClass.requiredMethod(nme.applyOrElse)

  @tu lazy val AbstractPartialFunctionClass: ClassSymbol = ctx.requiredClass("scala.runtime.AbstractPartialFunction")
  @tu lazy val FunctionXXLClass: ClassSymbol = ctx.requiredClass("scala.FunctionXXL")
  @tu lazy val ScalaSymbolClass: ClassSymbol = ctx.requiredClass("scala.Symbol")
  @tu lazy val DynamicClass: ClassSymbol = ctx.requiredClass("scala.Dynamic")
  @tu lazy val OptionClass: ClassSymbol = ctx.requiredClass("scala.Option")
  @tu lazy val SomeClass: ClassSymbol = ctx.requiredClass("scala.Some")
  @tu lazy val NoneModule: Symbol = ctx.requiredModule("scala.None")

  @tu lazy val EnumClass: ClassSymbol = ctx.requiredClass("scala.Enum")
    @tu lazy val Enum_ordinal: Symbol = EnumClass.requiredMethod(nme.ordinal)

  @tu lazy val EnumValuesClass: ClassSymbol = ctx.requiredClass("scala.runtime.EnumValues")
  @tu lazy val ProductClass: ClassSymbol = ctx.requiredClass("scala.Product")
    @tu lazy val Product_canEqual          : Symbol = ProductClass.requiredMethod(nme.canEqual_)
    @tu lazy val Product_productArity      : Symbol = ProductClass.requiredMethod(nme.productArity)
    @tu lazy val Product_productElement    : Symbol = ProductClass.requiredMethod(nme.productElement)
    @tu lazy val Product_productElementName: Symbol = ProductClass.requiredMethod(nme.productElementName)
    @tu lazy val Product_productPrefix     : Symbol = ProductClass.requiredMethod(nme.productPrefix)

  @tu lazy val IteratorClass: ClassSymbol = ctx.requiredClass("scala.collection.Iterator")
  def IteratorModule(implicit ctx: Context): Symbol = IteratorClass.companionModule

  @tu lazy val ModuleSerializationProxyClass: ClassSymbol = ctx.requiredClass("scala.runtime.ModuleSerializationProxy")
    @tu lazy val ModuleSerializationProxyConstructor: TermSymbol =
      ModuleSerializationProxyClass.requiredMethod(nme.CONSTRUCTOR, List(ClassType(TypeBounds.empty)))

  @tu lazy val MirrorClass: ClassSymbol = ctx.requiredClass("scala.deriving.Mirror")
  @tu lazy val Mirror_ProductClass: ClassSymbol = ctx.requiredClass("scala.deriving.Mirror.Product")
    @tu lazy val Mirror_Product_fromProduct: Symbol = Mirror_ProductClass.requiredMethod(nme.fromProduct)
  @tu lazy val Mirror_SumClass: ClassSymbol = ctx.requiredClass("scala.deriving.Mirror.Sum")
  @tu lazy val Mirror_SingletonClass: ClassSymbol = ctx.requiredClass("scala.deriving.Mirror.Singleton")
  @tu lazy val Mirror_SingletonProxyClass: ClassSymbol = ctx.requiredClass("scala.deriving.Mirror.SingletonProxy")

  @tu lazy val LanguageModule: Symbol = ctx.requiredModule("scala.language")
  @tu lazy val NonLocalReturnControlClass: ClassSymbol = ctx.requiredClass("scala.runtime.NonLocalReturnControl")
  @tu lazy val SelectableClass: ClassSymbol = ctx.requiredClass("scala.Selectable")

  @tu lazy val ClassTagClass: ClassSymbol = ctx.requiredClass("scala.reflect.ClassTag")
  @tu lazy val ClassTagModule: Symbol = ClassTagClass.companionModule
    @tu lazy val ClassTagModule_apply: Symbol = ClassTagModule.requiredMethod(nme.apply)

  @tu lazy val QuotedExprClass: ClassSymbol = ctx.requiredClass("scala.quoted.Expr")
  @tu lazy val QuotedExprModule: Symbol = QuotedExprClass.companionModule
    @tu lazy val QuotedExprModule_nullExpr: Symbol = QuotedExprModule.requiredMethod(nme.nullExpr)
    @tu lazy val QuotedExprModule_unitExpr: Symbol = QuotedExprModule.requiredMethod(nme.unitExpr)

  @tu lazy val QuoteContextClass: ClassSymbol = ctx.requiredClass("scala.quoted.QuoteContext")
  @tu lazy val QuoteContextModule: Symbol = QuoteContextClass.companionModule
    @tu lazy val QuoteContext_macroContext: Symbol = QuoteContextModule.requiredMethod("macroContext")

  @tu lazy val LiftableModule: Symbol = ctx.requiredModule("scala.quoted.Liftable")
    @tu lazy val LiftableModule_BooleanIsLiftable: Symbol = LiftableModule.requiredMethod("BooleanIsLiftable")
    @tu lazy val LiftableModule_ByteIsLiftable: Symbol = LiftableModule.requiredMethod("ByteIsLiftable")
    @tu lazy val LiftableModule_ShortIsLiftable: Symbol = LiftableModule.requiredMethod("ShortIsLiftable")
    @tu lazy val LiftableModule_IntIsLiftable: Symbol = LiftableModule.requiredMethod("IntIsLiftable")
    @tu lazy val LiftableModule_LongIsLiftable: Symbol = LiftableModule.requiredMethod("LongIsLiftable")
    @tu lazy val LiftableModule_FloatIsLiftable: Symbol = LiftableModule.requiredMethod("FloatIsLiftable")
    @tu lazy val LiftableModule_DoubleIsLiftable: Symbol = LiftableModule.requiredMethod("DoubleIsLiftable")
    @tu lazy val LiftableModule_CharIsLiftable: Symbol = LiftableModule.requiredMethod("CharIsLiftable")
    @tu lazy val LiftableModule_StringIsLiftable: Symbol = LiftableModule.requiredMethod("StringIsLiftable")

  @tu lazy val InternalQuotedModule: Symbol = ctx.requiredModule("scala.internal.Quoted")
    @tu lazy val InternalQuoted_exprQuote  : Symbol = InternalQuotedModule.requiredMethod("exprQuote")
    @tu lazy val InternalQuoted_exprSplice : Symbol = InternalQuotedModule.requiredMethod("exprSplice")
    @tu lazy val InternalQuoted_typeQuote  : Symbol = InternalQuotedModule.requiredMethod("typeQuote")
    @tu lazy val InternalQuoted_patternHole: Symbol = InternalQuotedModule.requiredMethod("patternHole")
    @tu lazy val InternalQuoted_patternBindHoleAnnot: ClassSymbol = InternalQuotedModule.requiredClass("patternBindHole")
    @tu lazy val InternalQuoted_QuoteTypeTagAnnot: ClassSymbol = InternalQuotedModule.requiredClass("quoteTypeTag")
    @tu lazy val InternalQuoted_fromAboveAnnot: ClassSymbol = InternalQuotedModule.requiredClass("fromAbove")


  @tu lazy val InternalQuotedExprModule: Symbol = ctx.requiredModule("scala.internal.quoted.Expr")
    @tu lazy val InternalQuotedExpr_unapply: Symbol = InternalQuotedExprModule.requiredMethod(nme.unapply)

  @tu lazy val InternalQuotedTypeModule: Symbol = ctx.requiredModule("scala.internal.quoted.Type")
    @tu lazy val InternalQuotedType_unapply: Symbol = InternalQuotedTypeModule.requiredMethod(nme.unapply)

  @tu lazy val QuotedTypeClass: ClassSymbol = ctx.requiredClass("scala.quoted.Type")
    @tu lazy val QuotedType_splice: Symbol = QuotedTypeClass.requiredType(tpnme.splice)

  @tu lazy val QuotedTypeModule: Symbol = QuotedTypeClass.companionModule

  @tu lazy val QuotedMatchingSymClass: ClassSymbol = ctx.requiredClass("scala.quoted.matching.Sym")
  @tu lazy val TastyReflectionClass: ClassSymbol = ctx.requiredClass("scala.tasty.Reflection")

  @tu lazy val Unpickler_unpickleExpr: Symbol = ctx.requiredMethod("scala.runtime.quoted.Unpickler.unpickleExpr")
  @tu lazy val Unpickler_unpickleType: Symbol = ctx.requiredMethod("scala.runtime.quoted.Unpickler.unpickleType")

  @tu lazy val EqlClass: ClassSymbol = ctx.requiredClass("scala.Eql")
    def Eql_eqlAny(implicit ctx: Context): TermSymbol = EqlClass.companionModule.requiredMethod(nme.eqlAny)

  @tu lazy val TypeBoxClass: ClassSymbol = ctx.requiredClass("scala.internal.TypeBox")
    @tu lazy val TypeBox_CAP: TypeSymbol = TypeBoxClass.requiredType(tpnme.CAP)

  @tu lazy val MatchCaseClass: ClassSymbol = ctx.requiredClass("scala.internal.MatchCase")
  @tu lazy val NotClass: ClassSymbol = ctx.requiredClass("scala.implicits.Not")
    @tu lazy val Not_value: Symbol = NotClass.companionModule.requiredMethod(nme.value)

  @tu lazy val ValueOfClass: ClassSymbol = ctx.requiredClass("scala.ValueOf")
  @tu lazy val StatsModule: Symbol = ctx.requiredModule("dotty.tools.dotc.util.Stats")
    @tu lazy val Stats_doRecord: Symbol = StatsModule.requiredMethod("doRecord")

  @tu lazy val FromDigitsClass: ClassSymbol           = ctx.requiredClass("scala.util.FromDigits")
  @tu lazy val FromDigits_WithRadixClass: ClassSymbol = ctx.requiredClass("scala.util.FromDigits.WithRadix")
  @tu lazy val FromDigits_DecimalClass: ClassSymbol   = ctx.requiredClass("scala.util.FromDigits.Decimal")
  @tu lazy val FromDigits_FloatingClass: ClassSymbol  = ctx.requiredClass("scala.util.FromDigits.Floating")

  @tu lazy val XMLTopScopeModule: Symbol = ctx.requiredModule("scala.xml.TopScope")

  @tu lazy val CommandLineParserModule: Symbol = ctx.requiredModule("scala.util.CommandLineParser")
    @tu lazy val CLP_ParseError: ClassSymbol = CommandLineParserModule.requiredClass("ParseError").typeRef.symbol.asClass
    @tu lazy val CLP_parseArgument: Symbol = CommandLineParserModule.requiredMethod("parseArgument")
    @tu lazy val CLP_parseRemainingArguments: Symbol = CommandLineParserModule.requiredMethod("parseRemainingArguments")
    @tu lazy val CLP_showError: Symbol = CommandLineParserModule.requiredMethod("showError")

  @tu lazy val TupleTypeRef: TypeRef = ctx.requiredClassRef("scala.Tuple")
  def TupleClass(implicit ctx: Context): ClassSymbol = TupleTypeRef.symbol.asClass
    @tu lazy val Tuple_cons: Symbol = TupleClass.requiredMethod("*:")
  @tu lazy val NonEmptyTupleTypeRef: TypeRef = ctx.requiredClassRef("scala.NonEmptyTuple")
  def NonEmptyTupleClass(implicit ctx: Context): ClassSymbol = NonEmptyTupleTypeRef.symbol.asClass
    lazy val NonEmptyTuple_tail: Symbol = NonEmptyTupleClass.requiredMethod("tail")

  @tu lazy val PairClass: ClassSymbol = ctx.requiredClass("scala.*:")
  @tu lazy val TupleXXLClass: ClassSymbol = ctx.requiredClass("scala.TupleXXL")
  def TupleXXLModule(implicit ctx: Context): Symbol = TupleXXLClass.companionModule

    def TupleXXL_fromIterator(implicit ctx: Context): Symbol = TupleXXLModule.requiredMethod("fromIterator")

  @tu lazy val DynamicTupleModule: Symbol = ctx.requiredModule("scala.runtime.DynamicTuple")
  @tu lazy val DynamicTupleModuleClass: Symbol = DynamicTupleModule.moduleClass
    lazy val DynamicTuple_consIterator: Symbol = DynamicTupleModule.requiredMethod("consIterator")
    lazy val DynamicTuple_concatIterator: Symbol = DynamicTupleModule.requiredMethod("concatIterator")
    lazy val DynamicTuple_dynamicApply: Symbol = DynamicTupleModule.requiredMethod("dynamicApply")
    lazy val DynamicTuple_dynamicCons: Symbol = DynamicTupleModule.requiredMethod("dynamicCons")
    lazy val DynamicTuple_dynamicSize: Symbol = DynamicTupleModule.requiredMethod("dynamicSize")
    lazy val DynamicTuple_dynamicTail: Symbol = DynamicTupleModule.requiredMethod("dynamicTail")
    lazy val DynamicTuple_dynamicConcat: Symbol = DynamicTupleModule.requiredMethod("dynamicConcat")
    lazy val DynamicTuple_dynamicToArray: Symbol = DynamicTupleModule.requiredMethod("dynamicToArray")
    lazy val DynamicTuple_productToArray: Symbol = DynamicTupleModule.requiredMethod("productToArray")

  @tu lazy val TupledFunctionTypeRef: TypeRef = ctx.requiredClassRef("scala.TupledFunction")
  def TupledFunctionClass(implicit ctx: Context): ClassSymbol = TupledFunctionTypeRef.symbol.asClass

  @tu lazy val InternalTupledFunctionTypeRef: TypeRef = ctx.requiredClassRef("scala.internal.TupledFunction")
  def InternalTupleFunctionClass(implicit ctx: Context): ClassSymbol = InternalTupledFunctionTypeRef.symbol.asClass
  def InternalTupleFunctionModule(implicit ctx: Context): Symbol = ctx.requiredModule("scala.internal.TupledFunction")

  // Annotation base classes
  @tu lazy val AnnotationClass: ClassSymbol = ctx.requiredClass("scala.annotation.Annotation")
  @tu lazy val ClassfileAnnotationClass: ClassSymbol = ctx.requiredClass("scala.annotation.ClassfileAnnotation")
  @tu lazy val StaticAnnotationClass: ClassSymbol = ctx.requiredClass("scala.annotation.StaticAnnotation")
  @tu lazy val RefiningAnnotationClass: ClassSymbol = ctx.requiredClass("scala.annotation.RefiningAnnotation")

  // Annotation classes
  @tu lazy val AliasAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.Alias")
  @tu lazy val AnnotationDefaultAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.AnnotationDefault")
  @tu lazy val BodyAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.Body")
  @tu lazy val ChildAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.Child")
  @tu lazy val WithBoundsAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.WithBounds")
  @tu lazy val CovariantBetweenAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.CovariantBetween")
  @tu lazy val ContravariantBetweenAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.ContravariantBetween")
  @tu lazy val DeprecatedAnnot: ClassSymbol = ctx.requiredClass("scala.deprecated")
  @tu lazy val ImplicitAmbiguousAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.implicitAmbiguous")
  @tu lazy val ImplicitNotFoundAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.implicitNotFound")
  @tu lazy val InlineParamAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.InlineParam")
  @tu lazy val InvariantBetweenAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.InvariantBetween")
  @tu lazy val MainAnnot: ClassSymbol = ctx.requiredClass("scala.main")
  @tu lazy val MigrationAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.migration")
  @tu lazy val NativeAnnot: ClassSymbol = ctx.requiredClass("scala.native")
  @tu lazy val RepeatedAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.Repeated")
  @tu lazy val SourceFileAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.SourceFile")
  @tu lazy val ScalaSignatureAnnot: ClassSymbol = ctx.requiredClass("scala.reflect.ScalaSignature")
  @tu lazy val ScalaLongSignatureAnnot: ClassSymbol = ctx.requiredClass("scala.reflect.ScalaLongSignature")
  @tu lazy val ScalaStrictFPAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.strictfp")
  @tu lazy val ScalaStaticAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.static")
  @tu lazy val SerialVersionUIDAnnot: ClassSymbol = ctx.requiredClass("scala.SerialVersionUID")
  @tu lazy val TASTYSignatureAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.TASTYSignature")
  @tu lazy val TASTYLongSignatureAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.internal.TASTYLongSignature")
  @tu lazy val TailrecAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.tailrec")
  @tu lazy val ThreadUnsafeAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.threadUnsafe")
  @tu lazy val TransientParamAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.constructorOnly")
  @tu lazy val CompileTimeOnlyAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.compileTimeOnly")
  @tu lazy val SwitchAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.switch")
  @tu lazy val ThrowsAnnot: ClassSymbol = ctx.requiredClass("scala.throws")
  @tu lazy val TransientAnnot: ClassSymbol = ctx.requiredClass("scala.transient")
  @tu lazy val UncheckedAnnot: ClassSymbol = ctx.requiredClass("scala.unchecked")
  @tu lazy val UncheckedStableAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.unchecked.uncheckedStable")
  @tu lazy val UncheckedVarianceAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.unchecked.uncheckedVariance")
  @tu lazy val VolatileAnnot: ClassSymbol = ctx.requiredClass("scala.volatile")
  @tu lazy val FieldMetaAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.meta.field")
  @tu lazy val GetterMetaAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.meta.getter")
  @tu lazy val SetterMetaAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.meta.setter")
  @tu lazy val ShowAsInfixAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.showAsInfix")
  @tu lazy val FunctionalInterfaceAnnot: ClassSymbol = ctx.requiredClass("java.lang.FunctionalInterface")
  @tu lazy val InfixAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.infix")
  @tu lazy val AlphaAnnot: ClassSymbol = ctx.requiredClass("scala.annotation.alpha")

  // A list of annotations that are commonly used to indicate that a field/method argument or return
  // type is not null. These annotations are used by the nullification logic in JavaNullInterop to
  // improve the precision of type nullification.
  // We don't require that any of these annotations be present in the class path, but we want to
  // create Symbols for the ones that are present, so they can be checked during nullification.
  @tu lazy val NotNullAnnots: List[ClassSymbol] = ctx.getClassesIfDefined(
    "javax.annotation.Nonnull" ::
    "edu.umd.cs.findbugs.annotations.NonNull" ::
    "androidx.annotation.NonNull" ::
    "android.support.annotation.NonNull" ::
    "android.annotation.NonNull" ::
    "com.android.annotations.NonNull" ::
    "org.eclipse.jdt.annotation.NonNull" ::
    "org.checkerframework.checker.nullness.qual.NonNull" ::
    "org.checkerframework.checker.nullness.compatqual.NonNullDecl" ::
    "org.jetbrains.annotations.NotNull" ::
    "lombok.NonNull" ::
    "io.reactivex.annotations.NonNull" :: Nil map PreNamedString)

  // convenient one-parameter method types
  def methOfAny(tp: Type): MethodType = MethodType(List(AnyType), tp)
  def methOfAnyVal(tp: Type): MethodType = MethodType(List(AnyValType), tp)
  def methOfAnyRef(tp: Type): MethodType = MethodType(List(ObjectType), tp)

  // Derived types

  def RepeatedParamType: TypeRef = RepeatedParamClass.typeRef

  def ClassType(arg: Type)(implicit ctx: Context): Type = {
    val ctype = ClassClass.typeRef
    if (ctx.phase.erasedTypes) ctype else ctype.appliedTo(arg)
  }

  /** The enumeration type, goven a value of the enumeration */
  def EnumType(sym: Symbol)(implicit ctx: Context): TypeRef =
    // given (in java): "class A { enum E { VAL1 } }"
    //  - sym: the symbol of the actual enumeration value (VAL1)
    //  - .owner: the ModuleClassSymbol of the enumeration (object E)
    //  - .linkedClass: the ClassSymbol of the enumeration (class E)
    sym.owner.linkedClass.typeRef

  object FunctionOf {
    def apply(args: List[Type], resultType: Type, isContextual: Boolean = false, isErased: Boolean = false)(implicit ctx: Context): Type =
      FunctionType(args.length, isContextual, isErased).appliedTo(args ::: resultType :: Nil)
    def unapply(ft: Type)(implicit ctx: Context): Option[(List[Type], Type, Boolean, Boolean)] = {
      val tsym = ft.typeSymbol
      if (isFunctionClass(tsym)) {
        val targs = ft.dealias.argInfos
        if (targs.isEmpty) None
        else Some(targs.init, targs.last, tsym.name.isImplicitFunction, tsym.name.isErasedFunction)
      }
      else None
    }
  }

  object PartialFunctionOf {
    def apply(arg: Type, result: Type)(implicit ctx: Context): Type =
      PartialFunctionClass.typeRef.appliedTo(arg :: result :: Nil)
    def unapply(pft: Type)(implicit ctx: Context): Option[(Type, List[Type])] =
      if (pft.isRef(PartialFunctionClass)) {
        val targs = pft.dealias.argInfos
        if (targs.length == 2) Some((targs.head, targs.tail)) else None
      }
      else None
  }

  object ArrayOf {
    def apply(elem: Type)(implicit ctx: Context): Type =
      if (ctx.erasedTypes) JavaArrayType(elem)
      else ArrayType.appliedTo(elem :: Nil)
    def unapply(tp: Type)(implicit ctx: Context): Option[Type] = tp.dealias match {
      case AppliedType(at, arg :: Nil) if at isRef ArrayType.symbol => Some(arg)
      case _ => None
    }
  }

  object MatchCase {
    def apply(pat: Type, body: Type)(implicit ctx: Context): Type =
      MatchCaseClass.typeRef.appliedTo(pat, body)
    def unapply(tp: Type)(implicit ctx: Context): Option[(Type, Type)] = tp match {
      case AppliedType(tycon, pat :: body :: Nil) if tycon.isRef(MatchCaseClass) =>
        Some((pat, body))
      case _ =>
        None
    }
    def isInstance(tp: Type)(implicit ctx: Context): Boolean = tp match {
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
    def apply(elem: Type, ndims: Int)(implicit ctx: Context): Type =
      if (ndims == 0) elem else ArrayOf(apply(elem, ndims - 1))
    def unapply(tp: Type)(implicit ctx: Context): Option[(Type, Int)] = tp match {
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

  final def isCompiletime_S(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.name == tpnme.S && sym.owner == CompiletimePackageObject.moduleClass

  // ----- Symbol sets ---------------------------------------------------

  @tu lazy val AbstractFunctionType: Array[TypeRef] = mkArityArray("scala.runtime.AbstractFunction", MaxImplementedFunctionArity, 0)
  val AbstractFunctionClassPerRun: PerRun[Array[Symbol]] = new PerRun(implicit ctx => AbstractFunctionType.map(_.symbol.asClass))
  def AbstractFunctionClass(n: Int)(implicit ctx: Context): Symbol = AbstractFunctionClassPerRun()(ctx)(n)
  @tu private lazy val ImplementedFunctionType = mkArityArray("scala.Function", MaxImplementedFunctionArity, 0)
  def FunctionClassPerRun: PerRun[Array[Symbol]] = new PerRun(implicit ctx => ImplementedFunctionType.map(_.symbol.asClass))

  val LazyHolder: PerRun[Map[Symbol, Symbol]] = new PerRun({ implicit ctx =>
    def holderImpl(holderType: String) = ctx.requiredClass("scala.runtime." + holderType)
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

  def FunctionClass(n: Int, isContextual: Boolean = false, isErased: Boolean = false)(implicit ctx: Context): Symbol =
    if (isContextual && isErased)
      ctx.requiredClass("scala.ErasedImplicitFunction" + n.toString)
    else if (isContextual)
      ctx.requiredClass("scala.ImplicitFunction" + n.toString)
    else if (isErased)
      ctx.requiredClass("scala.ErasedFunction" + n.toString)
    else if (n <= MaxImplementedFunctionArity)
      FunctionClassPerRun()(ctx)(n)
    else
      ctx.requiredClass("scala.Function" + n.toString)

  @tu lazy val Function0_apply: Symbol = ImplementedFunctionType(0).symbol.requiredMethod(nme.apply)

  def FunctionType(n: Int, isContextual: Boolean = false, isErased: Boolean = false)(implicit ctx: Context): TypeRef =
    if (n <= MaxImplementedFunctionArity && (!isContextual || ctx.erasedTypes) && !isErased) ImplementedFunctionType(n)
    else FunctionClass(n, isContextual, isErased).typeRef

  lazy val PolyFunctionClass = ctx.requiredClass("scala.PolyFunction")
  def PolyFunctionType = PolyFunctionClass.typeRef

  /** If `cls` is a class in the scala package, its name, otherwise EmptyTypeName */
  def scalaClassName(cls: Symbol)(implicit ctx: Context): TypeName =
    if (cls.isClass && cls.owner == ScalaPackageClass) cls.asClass.name else EmptyTypeName

  /** If type `ref` refers to a class in the scala package, its name, otherwise EmptyTypeName */
  def scalaClassName(ref: Type)(implicit ctx: Context): TypeName = scalaClassName(ref.classSymbol)

  private def isVarArityClass(cls: Symbol, prefix: String) =
    cls.isClass && cls.owner.eq(ScalaPackageClass) &&
    cls.name.testSimple(name =>
      name.startsWith(prefix) &&
      name.length > prefix.length &&
      name.drop(prefix.length).forall(_.isDigit))

  def isBottomClass(cls: Symbol): Boolean =
    if (ctx.explicitNulls && !ctx.phase.erasedTypes) cls == NothingClass
    else isBottomClassAfterErasure(cls)

  def isBottomClassAfterErasure(cls: Symbol): Boolean = cls == NothingClass || cls == NullClass

  def isBottomType(tp: Type): Boolean =
    if (ctx.explicitNulls && !ctx.phase.erasedTypes) tp.derivesFrom(NothingClass)
    else isBottomTypeAfterErasure(tp)

  def isBottomTypeAfterErasure(tp: Type): Boolean =
    tp.derivesFrom(NothingClass) || tp.derivesFrom(NullClass)

  /** Is a function class.
   *   - FunctionXXL
   *   - FunctionN for N >= 0
   *   - ImplicitFunctionN for N >= 0
   *   - ErasedFunctionN for N > 0
   *   - ErasedImplicitFunctionN for N > 0
   */
  def isFunctionClass(cls: Symbol): Boolean = scalaClassName(cls).isFunction

  /** Is an implicit function class.
   *   - ImplicitFunctionN for N >= 0
   *   - ErasedImplicitFunctionN for N > 0
   */
  def isImplicitFunctionClass(cls: Symbol): Boolean = scalaClassName(cls).isImplicitFunction

  /** Is an erased function class.
   *   - ErasedFunctionN for N > 0
   *   - ErasedImplicitFunctionN for N > 0
   */
  def isErasedFunctionClass(cls: Symbol): Boolean = scalaClassName(cls).isErasedFunction

  /** Is either FunctionXXL or  a class that will be erased to FunctionXXL
   *   - FunctionXXL
   *   - FunctionN for N >= 22
   *   - ImplicitFunctionN for N >= 22
   */
  def isXXLFunctionClass(cls: Symbol): Boolean = {
    val name = scalaClassName(cls)
    (name eq tpnme.FunctionXXL) || name.functionArity > MaxImplementedFunctionArity
  }

  /** Is a synthetic function class
   *    - FunctionN for N > 22
   *    - ImplicitFunctionN for N >= 0
   *    - ErasedFunctionN for N > 0
   *    - ErasedImplicitFunctionN for N > 0
   */
  def isSyntheticFunctionClass(cls: Symbol): Boolean = scalaClassName(cls).isSyntheticFunction

  def isAbstractFunctionClass(cls: Symbol): Boolean = isVarArityClass(cls, str.AbstractFunction)
  def isTupleClass(cls: Symbol): Boolean = isVarArityClass(cls, str.Tuple)
  def isProductClass(cls: Symbol): Boolean = isVarArityClass(cls, str.Product)

  def isScalaShadowingPackageClass(cls: Symbol): Boolean =
    cls.name == tpnme.scalaShadowing && cls.owner == RootClass

  /** Returns the erased class of the function class `cls`
   *    - FunctionN for N > 22 becomes FunctionXXL
   *    - FunctionN for 22 > N >= 0 remains as FunctionN
   *    - ImplicitFunctionN for N > 22 becomes FunctionXXL
   *    - ImplicitFunctionN for N <= 22 becomes FunctionN
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
   *    - ImplicitFunctionN for N > 22 becomes FunctionXXL
   *    - ImplicitFunctionN for N <= 22 becomes FunctionN
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

  val StaticRootImportFns: List[RootRef] = List[RootRef](
    (() => JavaLangPackageVal.termRef, false),
    (() => ScalaPackageVal.termRef, false)
  )

  val PredefImportFns: List[RootRef] = List[RootRef](
    (() => ScalaPredefModule.termRef, true),
    (() => DottyPredefModule.termRef, false)
  )

  @tu lazy val RootImportFns: List[RootRef] =
    if (ctx.settings.YnoImports.value) Nil
    else if (ctx.settings.YnoPredef.value) StaticRootImportFns
    else StaticRootImportFns ++ PredefImportFns

  @tu lazy val ShadowableImportNames: Set[TermName] = Set("Predef", "DottyPredef").map(_.toTermName)
  @tu lazy val RootImportTypes: List[TermRef] = RootImportFns.map(_._1())

  /** Modules whose members are in the default namespace and their module classes */
  @tu lazy val UnqualifiedOwnerTypes: Set[NamedType] =
    RootImportTypes.toSet[NamedType] ++ RootImportTypes.map(_.symbol.moduleClass.typeRef)

  @tu lazy val NotRuntimeClasses: Set[Symbol] = Set(AnyClass, AnyValClass, NullClass, NothingClass)

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

  def isTupleType(tp: Type)(implicit ctx: Context): Boolean = {
    val arity = tp.dealias.argInfos.length
    arity <= MaxTupleArity && TupleType(arity) != null && (tp isRef TupleType(arity).symbol)
  }

  def tupleType(elems: List[Type]): Type = {
    val arity = elems.length
    if (0 < arity && arity <= MaxTupleArity && TupleType(arity) != null) TupleType(arity).appliedTo(elems)
    else TypeOps.nestedPairs(elems)
  }

  def tupleTypes(tp: Type, bound: Int = Int.MaxValue)(implicit ctx: Context): Option[List[Type]] = {
    @tailrec def rec(tp: Type, acc: List[Type], bound: Int): Option[List[Type]] = tp.normalized match {
      case _ if bound < 0 => Some(acc.reverse)
      case tp: AppliedType if defn.PairClass == tp.classSymbol => rec(tp.args(1), tp.args.head :: acc, bound - 1)
      case tp: AppliedType if defn.isTupleClass(tp.tycon.classSymbol) => Some(acc.reverse ::: tp.args)
      case tp if tp.classSymbol == defn.UnitClass => Some(acc.reverse)
      case _ => None
    }
    rec(tp.stripTypeVar, Nil, bound)
  }

  def isProductSubType(tp: Type)(implicit ctx: Context): Boolean = tp.derivesFrom(ProductClass)

  /** Is `tp` (an alias) of either a scala.FunctionN or a scala.ImplicitFunctionN
   *  instance?
   */
  def isNonRefinedFunction(tp: Type)(implicit ctx: Context): Boolean = {
    val arity = functionArity(tp)
    val sym = tp.dealias.typeSymbol

    arity >= 0 &&
      isFunctionClass(sym) &&
      tp.isRef(FunctionType(arity, sym.name.isImplicitFunction, sym.name.isErasedFunction).typeSymbol) &&
      !tp.isInstanceOf[RefinedType]
  }

  /** Is `tp` a representation of a (possibly depenent) function type or an alias of such? */
  def isFunctionType(tp: Type)(implicit ctx: Context): Boolean =
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
    new PerRun(implicit ctx => Function1SpecializedParamTypes.map(_.symbol))
  @tu lazy val Function2SpecializedParamClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function2SpecializedParamTypes.map(_.symbol))
  @tu lazy val Function0SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function0SpecializedReturnTypes.map(_.symbol))
  @tu lazy val Function1SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function1SpecializedReturnTypes.map(_.symbol))
  @tu lazy val Function2SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function2SpecializedReturnTypes.map(_.symbol))

  def isSpecializableFunction(cls: ClassSymbol, paramTypes: List[Type], retType: Type)(implicit ctx: Context): Boolean =
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

  def functionArity(tp: Type)(implicit ctx: Context): Int = tp.dropDependentRefinement.dealias.argInfos.length - 1

  /** Return underlying immplicit function type (i.e. instance of an ImplicitFunctionN class)
   *  or NoType if none exists. The following types are considered as underlying types:
   *   - the alias of an alias type
   *   - the instance or origin of a TypeVar (i.e. the result of a stripTypeVar)
   *   - the upper bound of a TypeParamRef in the current constraint
   */
  def asImplicitFunctionType(tp: Type)(implicit ctx: Context): Type =
    tp.stripTypeVar.dealias match {
      case tp1: TypeParamRef if ctx.typerState.constraint.contains(tp1) =>
        asImplicitFunctionType(ctx.typeComparer.bounds(tp1).hiBound)
      case tp1 =>
        if (isFunctionType(tp1) && tp1.typeSymbol.name.isImplicitFunction) tp1
        else NoType
    }

  /** Is `tp` an implicit function type? */
  def isImplicitFunctionType(tp: Type)(implicit ctx: Context): Boolean =
    asImplicitFunctionType(tp).exists

  def isErasedFunctionType(tp: Type)(implicit ctx: Context): Boolean =
    isFunctionType(tp) && tp.dealias.typeSymbol.name.isErasedFunction

  /** A whitelist of Scala-2 classes that are known to be pure */
  def isAssuredNoInits(sym: Symbol): Boolean =
    (sym `eq` SomeClass) || isTupleClass(sym)

  /** If `cls` is Tuple1..Tuple22, add the corresponding *: type as last parent to `parents` */
  def adjustForTuple(cls: ClassSymbol, tparams: List[TypeSymbol], parents: List[Type]): List[Type] = {
    def syntheticParent(tparams: List[TypeSymbol]): Type =
      if (tparams.isEmpty) TupleTypeRef
      else TypeOps.nestedPairs(tparams.map(_.typeRef))
    if (isTupleClass(cls) || cls == UnitClass) parents :+ syntheticParent(tparams)
    else parents
  }

  /** Is synthesized symbol with alphanumeric name allowed to be used as an infix operator? */
  def isInfix(sym: Symbol)(implicit ctx: Context): Boolean =
    (sym eq Object_eq) || (sym eq Object_ne)

  // ----- primitive value class machinery ------------------------------------------

  /** This class would also be obviated by the implicit function type design */
  class PerRun[T](generate: Context => T) {
    private var current: RunId = NoRunId
    private var cached: T = _
    def apply()(implicit ctx: Context): T = {
      if (current != ctx.runId) {
        cached = generate(ctx)
        current = ctx.runId
      }
      cached
    }
  }

  @tu lazy val ScalaNumericValueTypeList: List[TypeRef] = List(
    ByteType, ShortType, CharType, IntType, LongType, FloatType, DoubleType)

  @tu private lazy val ScalaNumericValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypeList.toSet
  @tu private lazy val ScalaValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypes `union` Set(UnitType, BooleanType)

  val ScalaNumericValueClasses: PerRun[collection.Set[Symbol]] = new PerRun(implicit ctx => ScalaNumericValueTypes.map(_.symbol))
  val ScalaValueClasses: PerRun[collection.Set[Symbol]]        = new PerRun(implicit ctx => ScalaValueTypes.map(_.symbol))

  val ScalaBoxedClasses: PerRun[collection.Set[Symbol]] = new PerRun(implicit ctx =>
    Set(BoxedByteClass, BoxedShortClass, BoxedCharClass, BoxedIntClass, BoxedLongClass, BoxedFloatClass, BoxedDoubleClass, BoxedUnitClass, BoxedBooleanClass)
  )

  private val valueTypeEnc = mutable.Map[TypeName, PrimitiveClassEnc]()
  private val typeTags = mutable.Map[TypeName, Name]().withDefaultValue(nme.specializedTypeNames.Object)

//  private val unboxedTypeRef = mutable.Map[TypeName, TypeRef]()
//  private val javaTypeToValueTypeRef = mutable.Map[Class[?], TypeRef]()
//  private val valueTypeNamesToJavaType = mutable.Map[TypeName, Class[?]]()

  private def valueTypeRef(name: String, jtype: Class[?], enc: Int, tag: Name): TypeRef = {
    val vcls = ctx.requiredClassRef(name)
    valueTypeEnc(vcls.name) = enc
    typeTags(vcls.name) = tag
//    unboxedTypeRef(boxed.name) = vcls
//    javaTypeToValueTypeRef(jtype) = vcls
//    valueTypeNamesToJavaType(vcls.name) = jtype
    vcls
  }

  /** The type of the boxed class corresponding to primitive value type `tp`. */
  def boxedType(tp: Type)(implicit ctx: Context): TypeRef = {
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
  def typeTag(tp: Type)(implicit ctx: Context): Name = typeTags(scalaClassName(tp))

//  /** The `Class[?]` of a primitive value type name */
//  def valueTypeNameToJavaType(name: TypeName)(implicit ctx: Context): Option[Class[?]] =
//    valueTypeNamesToJavaType.get(if (name.firstPart eq nme.scala_) name.lastPart.toTypeName else name)

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

  def isValueSubType(tref1: TypeRef, tref2: TypeRef)(implicit ctx: Context): Boolean =
    valueTypeEnc(tref2.name) % valueTypeEnc(tref1.name) == 0
  def isValueSubClass(sym1: Symbol, sym2: Symbol): Boolean =
    valueTypeEnc(sym2.asClass.name) % valueTypeEnc(sym1.asClass.name) == 0

  @tu lazy val specialErasure: SimpleIdentityMap[Symbol, ClassSymbol] =
    SimpleIdentityMap.Empty[Symbol]
      .updated(AnyClass, ObjectClass)
      .updated(AnyValClass, ObjectClass)
      .updated(SingletonClass, ObjectClass)
      .updated(TupleClass, ObjectClass)
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

    if (ctx.explicitNulls) synth :+ JavaNullAlias else synth
  }

  @tu lazy val syntheticCoreClasses: List[Symbol] = syntheticScalaClasses ++ List(
    EmptyPackageVal,
    OpsPackageClass)

  /** Lists core methods that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  @tu lazy val syntheticCoreMethods: List[TermSymbol] =
    AnyMethods ++ ObjectMethods ++ List(String_+, throwMethod)

  @tu lazy val reservedScalaClassNames: Set[Name] = syntheticScalaClasses.map(_.name).toSet

  private var isInitialized = false

  def init()(implicit ctx: Context): Unit = {
    this.ctx = ctx
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
