package dotty.tools
package dotc
package core

import scala.annotation.threadUnsafe
import Types._, Contexts._, Symbols._, SymDenotations._, StdNames._, Names._
import Flags._, Scopes._, Decorators._, NameOps._, Periods._
import unpickleScala2.Scala2Unpickler.ensureConstructor
import scala.collection.mutable
import collection.mutable
import Denotations.SingleDenotation
import util.SimpleIdentityMap

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
    ctx.newClassSymbol(owner, name, flags | Permanent | NoInits, infoFn)

  private def enterCompleteClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet, parents: List[TypeRef], decls: Scope = newScope) =
    ctx.newCompleteClassSymbol(owner, name, flags | Permanent | NoInits, parents, decls).entered

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
   *        def apply given ($x0: T0, ..., $x{N_1}: T{N-1}): R
   *      }
   *
   *  ErasedFunctionN traits follow this template:
   *
   *      trait ErasedFunctionN[T0,...,T{N-1}, R] extends Object {
   *        def apply erased ($x0: T0, ..., $x{N_1}: T{N-1}): R
   *      }
   *
   *  ErasedImplicitFunctionN traits follow this template:
   *
   *      trait ErasedImplicitFunctionN[T0,...,T{N-1}, R] extends Object {
   *        def apply given erased ($x0: T0, ..., $x{N_1}: T{N-1}): R
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
    newClassSymbol(ScalaPackageClass, name, Trait | NoInits, completer)
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
                    resultTypeFn: PolyType => Type, flags: FlagSet = EmptyFlags,
                    useCompleter: Boolean = false) = {
    val tparamNames = PolyType.syntheticParamNames(typeParamCount)
    val tparamInfos = tparamNames map (_ => TypeBounds.empty)
    def ptype = PolyType(tparamNames)(_ => tparamInfos, resultTypeFn)
    val info =
      if (useCompleter)
        new LazyType {
          def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
            denot.info = ptype
          }
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

  @threadUnsafe lazy val RootClass: ClassSymbol = ctx.newPackageSymbol(
    NoSymbol, nme.ROOT, (root, rootcls) => ctx.base.rootLoader(root)).moduleClass.asClass
  @threadUnsafe lazy val RootPackage: TermSymbol = ctx.newSymbol(
    NoSymbol, nme.ROOTPKG, PackageCreationFlags, TypeRef(NoPrefix, RootClass))

  @threadUnsafe lazy val EmptyPackageVal: TermSymbol = ctx.newPackageSymbol(
    RootClass, nme.EMPTY_PACKAGE, (emptypkg, emptycls) => ctx.base.rootLoader(emptypkg)).entered
  @threadUnsafe lazy val EmptyPackageClass: ClassSymbol = EmptyPackageVal.moduleClass.asClass

  /** A package in which we can place all methods that are interpreted specially by the compiler */
  @threadUnsafe lazy val OpsPackageVal: TermSymbol = ctx.newCompletePackageSymbol(RootClass, nme.OPS_PACKAGE).entered
  @threadUnsafe lazy val OpsPackageClass: ClassSymbol = OpsPackageVal.moduleClass.asClass

  @threadUnsafe lazy val ScalaPackageVal: TermSymbol = ctx.requiredPackage(nme.scala_)
  @threadUnsafe lazy val ScalaMathPackageVal: TermSymbol = ctx.requiredPackage("scala.math")
  @threadUnsafe lazy val ScalaPackageClass: ClassSymbol = {
    val cls = ScalaPackageVal.moduleClass.asClass
    cls.info.decls.openForMutations.useSynthesizer(
      name => ctx =>
        if (name.isTypeName && name.isSyntheticFunction) newFunctionNTrait(name.asTypeName)
        else NoSymbol)
    cls
  }
  @threadUnsafe lazy val ScalaPackageObject: Symbol = ctx.requiredModuleRef("scala.package").symbol
  @threadUnsafe lazy val JavaPackageVal: TermSymbol = ctx.requiredPackage(nme.java)
  @threadUnsafe lazy val JavaLangPackageVal: TermSymbol = ctx.requiredPackage(jnme.JavaLang)

  // fundamental modules
  @threadUnsafe lazy val SysPackage : Symbol = ctx.requiredModuleRef("scala.sys.package").symbol
    @threadUnsafe lazy val Sys_error: Symbol = SysPackage.moduleClass.requiredMethodRef(nme.error).symbol

  @threadUnsafe lazy val ScalaXmlPackageClass: Symbol = ctx.getPackageClassIfDefined("scala.xml")

  @threadUnsafe lazy val CompiletimePackageObject: Symbol = ctx.requiredModuleRef("scala.compiletime.package").symbol
    @threadUnsafe lazy val Compiletime_error        : Symbol = CompiletimePackageObject.requiredMethodRef(nme.error).symbol
    @threadUnsafe lazy val Compiletime_constValue   : Symbol = CompiletimePackageObject.requiredMethodRef("constValue").symbol
    @threadUnsafe lazy val Compiletime_constValueOpt: Symbol = CompiletimePackageObject.requiredMethodRef("constValueOpt").symbol
    @threadUnsafe lazy val Compiletime_code         : Symbol = CompiletimePackageObject.requiredMethodRef("code").symbol

  /** The `scalaShadowing` package is used to safely modify classes and
   *  objects in scala so that they can be used from dotty. They will
   *  be visible as members of the `scala` package, replacing any objects
   *  or classes with the same name. But their binary artifacts are
   *  in `scalaShadowing` so they don't clash with the same-named `scala`
   *  members at runtime.
   */
  @threadUnsafe lazy val ScalaShadowingPackage: TermSymbol = ctx.requiredPackage(nme.scalaShadowing)

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
   *
   * There's a remaining question about `getClass`. In Scala2.x `getClass` was handled by compiler magic.
   * This is deemed too cumersome for Dotty and therefore right now `getClass` gets no special treatment;
   * it's just a method on `Any` which returns the raw type `java.lang.Class`. An alternative
   * way to get better `getClass` typing would be to treat `getClass` as a method of a generic
   * decorator which gets remapped in a later phase to Object#getClass. Then we could give it
   * the right type without changing the typechecker:
   *
   *     implicit class AnyGetClass[T](val x: T) extends AnyVal {
   *       def getClass: java.lang.Class[T] = ???
   *     }
   */
  @threadUnsafe lazy val AnyClass: ClassSymbol = completeClass(enterCompleteClassSymbol(ScalaPackageClass, tpnme.Any, Abstract, Nil), ensureCtor = false)
  def AnyType: TypeRef = AnyClass.typeRef
  @threadUnsafe lazy val AnyValClass: ClassSymbol = completeClass(enterCompleteClassSymbol(ScalaPackageClass, tpnme.AnyVal, Abstract, List(AnyClass.typeRef)))
  def AnyValType: TypeRef = AnyValClass.typeRef

    @threadUnsafe lazy val Any_== : TermSymbol       = enterMethod(AnyClass, nme.EQ, methOfAny(BooleanType), Final)
    @threadUnsafe lazy val Any_!= : TermSymbol       = enterMethod(AnyClass, nme.NE, methOfAny(BooleanType), Final)
    @threadUnsafe lazy val Any_equals: TermSymbol   = enterMethod(AnyClass, nme.equals_, methOfAny(BooleanType))
    @threadUnsafe lazy val Any_hashCode: TermSymbol = enterMethod(AnyClass, nme.hashCode_, MethodType(Nil, IntType))
    @threadUnsafe lazy val Any_toString: TermSymbol = enterMethod(AnyClass, nme.toString_, MethodType(Nil, StringType))
    @threadUnsafe lazy val Any_## : TermSymbol       = enterMethod(AnyClass, nme.HASHHASH, ExprType(IntType), Final)
    @threadUnsafe lazy val Any_getClass: TermSymbol = enterMethod(AnyClass, nme.getClass_, MethodType(Nil, ClassClass.typeRef.appliedTo(TypeBounds.empty)), Final)
    @threadUnsafe lazy val Any_isInstanceOf: TermSymbol = enterT1ParameterlessMethod(AnyClass, nme.isInstanceOf_, _ => BooleanType, Final)
    @threadUnsafe lazy val Any_asInstanceOf: TermSymbol = enterT1ParameterlessMethod(AnyClass, nme.asInstanceOf_, _.paramRefs(0), Final)
    @threadUnsafe lazy val Any_typeTest: TermSymbol = enterT1ParameterlessMethod(AnyClass, nme.isInstanceOfPM, _ => BooleanType, Final | Synthetic | Artifact)
    @threadUnsafe lazy val Any_typeCast: TermSymbol = enterT1ParameterlessMethod(AnyClass, nme.asInstanceOfPM, _.paramRefs(0), Final | Synthetic | Artifact | StableRealizable)
      // generated by pattern matcher, eliminated by erasure

    def AnyMethods: List[TermSymbol] = List(Any_==, Any_!=, Any_equals, Any_hashCode,
      Any_toString, Any_##, Any_getClass, Any_isInstanceOf, Any_asInstanceOf, Any_typeTest, Any_typeCast)

  @threadUnsafe lazy val ObjectClass: ClassSymbol = {
    val cls = ctx.requiredClass("java.lang.Object")
    assert(!cls.isCompleted, "race for completing java.lang.Object")
    cls.info = ClassInfo(cls.owner.thisType, cls, AnyClass.typeRef :: Nil, newScope)
    cls.setFlag(NoInits)

    // The companion object doesn't really exist, so it needs to be marked as
    // absent. Here we need to set it before completing attempt to load Object's
    // classfile, which causes issue #1648.
    val companion = JavaLangPackageVal.info.decl(nme.Object).symbol
    companion.moduleClass.markAbsent()
    companion.markAbsent()

    completeClass(cls)
  }
  def ObjectType: TypeRef = ObjectClass.typeRef

  @threadUnsafe lazy val AnyRefAlias: TypeSymbol = enterAliasType(tpnme.AnyRef, ObjectType)
  def AnyRefType: TypeRef = AnyRefAlias.typeRef

    @threadUnsafe lazy val Object_eq: TermSymbol = enterMethod(ObjectClass, nme.eq, methOfAnyRef(BooleanType), Final)
    @threadUnsafe lazy val Object_ne: TermSymbol = enterMethod(ObjectClass, nme.ne, methOfAnyRef(BooleanType), Final)
    @threadUnsafe lazy val Object_synchronized: TermSymbol = enterPolyMethod(ObjectClass, nme.synchronized_, 1,
        pt => MethodType(List(pt.paramRefs(0)), pt.paramRefs(0)), Final)
    @threadUnsafe lazy val Object_clone: TermSymbol = enterMethod(ObjectClass, nme.clone_, MethodType(Nil, ObjectType), Protected)
    @threadUnsafe lazy val Object_finalize: TermSymbol = enterMethod(ObjectClass, nme.finalize_, MethodType(Nil, UnitType), Protected)
    @threadUnsafe lazy val Object_notify: TermSymbol = enterMethod(ObjectClass, nme.notify_, MethodType(Nil, UnitType), Final)
    @threadUnsafe lazy val Object_notifyAll: TermSymbol = enterMethod(ObjectClass, nme.notifyAll_, MethodType(Nil, UnitType), Final)
    @threadUnsafe lazy val Object_wait: TermSymbol = enterMethod(ObjectClass, nme.wait_, MethodType(Nil, UnitType), Final)
    @threadUnsafe lazy val Object_waitL: TermSymbol = enterMethod(ObjectClass, nme.wait_, MethodType(LongType :: Nil, UnitType), Final)
    @threadUnsafe lazy val Object_waitLI: TermSymbol = enterMethod(ObjectClass, nme.wait_, MethodType(LongType :: IntType :: Nil, UnitType), Final)

    def ObjectMethods: List[TermSymbol] = List(Object_eq, Object_ne, Object_synchronized, Object_clone,
        Object_finalize, Object_notify, Object_notifyAll, Object_wait, Object_waitL, Object_waitLI)

  @threadUnsafe lazy val AnyKindClass: ClassSymbol = {
    val cls = ctx.newCompleteClassSymbol(ScalaPackageClass, tpnme.AnyKind, AbstractFinal | Permanent, Nil)
    if (!ctx.settings.YnoKindPolymorphism.value) {
      // Enable kind-polymorphism by exposing scala.AnyKind
      cls.entered
    }
    cls
  }
  def AnyKindType: TypeRef = AnyKindClass.typeRef

  @threadUnsafe lazy val andType: TypeSymbol = enterBinaryAlias(tpnme.AND, AndType(_, _))
  @threadUnsafe lazy val orType: TypeSymbol = enterBinaryAlias(tpnme.OR, OrType(_, _))

  /** Marker method to indicate an argument to a call-by-name parameter.
   *  Created by byNameClosures and elimByName, eliminated by Erasure,
   */
  @threadUnsafe lazy val cbnArg: TermSymbol = enterPolyMethod(OpsPackageClass, nme.cbnArg, 1,
      pt => MethodType(List(FunctionOf(Nil, pt.paramRefs(0))), pt.paramRefs(0)))

  /** Method representing a throw */
  @threadUnsafe lazy val throwMethod: TermSymbol = enterMethod(OpsPackageClass, nme.THROWkw,
      MethodType(List(ThrowableType), NothingType))

  @threadUnsafe lazy val NothingClass: ClassSymbol = enterCompleteClassSymbol(
    ScalaPackageClass, tpnme.Nothing, AbstractFinal, List(AnyClass.typeRef))
  def NothingType: TypeRef = NothingClass.typeRef
  @threadUnsafe lazy val RuntimeNothingModuleRef: TermRef = ctx.requiredModuleRef("scala.runtime.Nothing")
  @threadUnsafe lazy val NullClass: ClassSymbol = enterCompleteClassSymbol(
    ScalaPackageClass, tpnme.Null, AbstractFinal, List(ObjectClass.typeRef))
  def NullType: TypeRef = NullClass.typeRef
  @threadUnsafe lazy val RuntimeNullModuleRef: TermRef = ctx.requiredModuleRef("scala.runtime.Null")

  @threadUnsafe lazy val ImplicitScrutineeTypeSym =
    newSymbol(ScalaPackageClass, tpnme.IMPLICITkw, EmptyFlags, TypeBounds.empty).entered
  def ImplicitScrutineeTypeRef: TypeRef = ImplicitScrutineeTypeSym.typeRef


  @threadUnsafe lazy val ScalaPredefModule: Symbol = ctx.requiredModuleRef("scala.Predef").symbol
    @threadUnsafe lazy val Predef_conforms : Symbol = ScalaPredefModule.requiredMethodRef(nme.conforms_).symbol
    @threadUnsafe lazy val Predef_classOf  : Symbol = ScalaPredefModule.requiredMethodRef(nme.classOf).symbol
    @threadUnsafe lazy val Predef_undefined: Symbol = ScalaPredefModule.requiredMethodRef(nme.???).symbol

  def SubTypeClass(implicit ctx: Context): ClassSymbol =
    if (isNewCollections)
      ctx.requiredClass("scala.<:<")
    else
      ScalaPredefModule.requiredClass("<:<")

  def DummyImplicitClass(implicit ctx: Context): ClassSymbol =
    if (isNewCollections)
      ctx.requiredClass("scala.DummyImplicit")
    else
      ScalaPredefModule.requiredClass("DummyImplicit")

  @threadUnsafe lazy val ScalaRuntimeModule: Symbol = ctx.requiredModuleRef("scala.runtime.ScalaRunTime").symbol
    def runtimeMethodRef(name: PreName): TermRef = ScalaRuntimeModule.requiredMethodRef(name)
    def ScalaRuntime_drop: Symbol = runtimeMethodRef(nme.drop).symbol

  @threadUnsafe lazy val BoxesRunTimeModule: Symbol = ctx.requiredModuleRef("scala.runtime.BoxesRunTime").symbol
  @threadUnsafe lazy val ScalaStaticsModule: Symbol = ctx.requiredModuleRef("scala.runtime.Statics").symbol
    def staticsMethodRef(name: PreName): TermRef = ScalaStaticsModule.requiredMethodRef(name)
    def staticsMethod(name: PreName): TermSymbol = ScalaStaticsModule.requiredMethod(name)

  // Dotty deviation: we cannot use a @threadUnsafe lazy val here because @threadUnsafe lazy vals in dotty
  // will return "null" when called recursively, see #1856.
  def DottyPredefModule: Symbol = {
    if (myDottyPredefModule == null) {
      myDottyPredefModule = ctx.requiredModule("dotty.DottyPredef")
      assert(myDottyPredefModule != null)
    }
    myDottyPredefModule
  }
  private[this] var myDottyPredefModule: Symbol = _

  @threadUnsafe lazy val DottyArraysModule: Symbol = ctx.requiredModuleRef("dotty.runtime.Arrays").symbol
    def newGenericArrayMethod(implicit ctx: Context): TermSymbol = DottyArraysModule.requiredMethod("newGenericArray")
    def newArrayMethod(implicit ctx: Context): TermSymbol = DottyArraysModule.requiredMethod("newArray")

  // TODO: Remove once we drop support for 2.12 standard library
  @threadUnsafe lazy val isNewCollections: Boolean = ctx.settings.YnewCollections.value

  def getWrapVarargsArrayModule: Symbol = if (isNewCollections) ScalaRuntimeModule else ScalaPredefModule

  // The set of all wrap{X, Ref}Array methods, where X is a value type
  val WrapArrayMethods: PerRun[collection.Set[Symbol]] = new PerRun({ implicit ctx =>
    val methodNames = ScalaValueTypes.map(ast.tpd.wrapArrayMethodName) + nme.wrapRefArray
    methodNames.map(getWrapVarargsArrayModule.requiredMethodRef(_).symbol)
  })

  @threadUnsafe lazy val NilModule: Symbol = ctx.requiredModuleRef("scala.collection.immutable.Nil").symbol

  @threadUnsafe lazy val SingletonClass: ClassSymbol =
    // needed as a synthetic class because Scala 2.x refers to it in classfiles
    // but does not define it as an explicit class.
    enterCompleteClassSymbol(
      ScalaPackageClass, tpnme.Singleton, PureInterfaceCreationFlags | Final,
      List(AnyClass.typeRef), EmptyScope)
  @threadUnsafe lazy val SingletonType: TypeRef = SingletonClass.typeRef

  @threadUnsafe lazy val SeqType: TypeRef =
    if (isNewCollections) ctx.requiredClassRef("scala.collection.immutable.Seq")
    else ctx.requiredClassRef("scala.collection.Seq")
  def SeqClass given Context: ClassSymbol = SeqType.symbol.asClass
    @threadUnsafe lazy val Seq_apply        : Symbol = SeqClass.requiredMethodRef(nme.apply).symbol
    @threadUnsafe lazy val Seq_head         : Symbol = SeqClass.requiredMethodRef(nme.head).symbol
    @threadUnsafe lazy val Seq_drop         : Symbol = SeqClass.requiredMethodRef(nme.drop).symbol
    @threadUnsafe lazy val Seq_lengthCompare: Symbol = SeqClass.requiredMethodRef(nme.lengthCompare, List(IntType)).symbol
    @threadUnsafe lazy val Seq_length       : Symbol = SeqClass.requiredMethodRef(nme.length).symbol
    @threadUnsafe lazy val Seq_toSeq        : Symbol = SeqClass.requiredMethodRef(nme.toSeq).symbol

  @threadUnsafe lazy val ArrayType: TypeRef = ctx.requiredClassRef("scala.Array")
  def ArrayClass given Context: ClassSymbol = ArrayType.symbol.asClass
    @threadUnsafe lazy val Array_apply     : Symbol = ArrayClass.requiredMethodRef(nme.apply).symbol
    @threadUnsafe lazy val Array_update    : Symbol = ArrayClass.requiredMethodRef(nme.update).symbol
    @threadUnsafe lazy val Array_length    : Symbol = ArrayClass.requiredMethodRef(nme.length).symbol
    @threadUnsafe lazy val Array_clone     : Symbol = ArrayClass.requiredMethodRef(nme.clone_).symbol
    @threadUnsafe lazy val ArrayConstructor: Symbol = ArrayClass.requiredMethodRef(nme.CONSTRUCTOR).symbol

  @threadUnsafe lazy val ArrayModule: Symbol = ctx.requiredModuleRef("scala.Array").symbol

  @threadUnsafe lazy val UnitType: TypeRef = valueTypeRef("scala.Unit", java.lang.Void.TYPE, UnitEnc, nme.specializedTypeNames.Void)
  def UnitClass given Context: ClassSymbol = UnitType.symbol.asClass
  def UnitModuleClass given Context: Symbol = UnitType.symbol.asClass.linkedClass
  @threadUnsafe lazy val BooleanType: TypeRef = valueTypeRef("scala.Boolean", java.lang.Boolean.TYPE, BooleanEnc, nme.specializedTypeNames.Boolean)
  def BooleanClass given Context: ClassSymbol = BooleanType.symbol.asClass
    @threadUnsafe lazy val Boolean_!  : Symbol = BooleanClass.requiredMethodRef(nme.UNARY_!).symbol
    @threadUnsafe lazy val Boolean_&& : Symbol = BooleanClass.requiredMethodRef(nme.ZAND).symbol // ### harmonize required... calls
    @threadUnsafe lazy val Boolean_|| : Symbol = BooleanClass.requiredMethodRef(nme.ZOR).symbol
    @threadUnsafe lazy val Boolean_== : Symbol =
      BooleanClass.info.member(nme.EQ).suchThat(_.info.firstParamTypes match {
        case List(pt) => (pt isRef BooleanClass)
        case _ => false
      }).symbol
    @threadUnsafe lazy val Boolean_!= : Symbol =
      BooleanClass.info.member(nme.NE).suchThat(_.info.firstParamTypes match {
        case List(pt) => (pt isRef BooleanClass)
        case _ => false
      }).symbol

  @threadUnsafe lazy val ByteType: TypeRef = valueTypeRef("scala.Byte", java.lang.Byte.TYPE, ByteEnc, nme.specializedTypeNames.Byte)
  def ByteClass given Context: ClassSymbol = ByteType.symbol.asClass
  @threadUnsafe lazy val ShortType: TypeRef = valueTypeRef("scala.Short", java.lang.Short.TYPE, ShortEnc, nme.specializedTypeNames.Short)
  def ShortClass given Context: ClassSymbol = ShortType.symbol.asClass
  @threadUnsafe lazy val CharType: TypeRef = valueTypeRef("scala.Char", java.lang.Character.TYPE, CharEnc, nme.specializedTypeNames.Char)
  def CharClass given Context: ClassSymbol = CharType.symbol.asClass
  @threadUnsafe lazy val IntType: TypeRef = valueTypeRef("scala.Int", java.lang.Integer.TYPE, IntEnc, nme.specializedTypeNames.Int)
  def IntClass given Context: ClassSymbol = IntType.symbol.asClass
    @threadUnsafe lazy val Int_-  : Symbol = IntClass.requiredMethodRef(nme.MINUS, List(IntType)).symbol
    @threadUnsafe lazy val Int_+  : Symbol = IntClass.requiredMethodRef(nme.PLUS, List(IntType)).symbol
    @threadUnsafe lazy val Int_/  : Symbol = IntClass.requiredMethodRef(nme.DIV, List(IntType)).symbol
    @threadUnsafe lazy val Int_*  : Symbol = IntClass.requiredMethodRef(nme.MUL, List(IntType)).symbol
    @threadUnsafe lazy val Int_== : Symbol = IntClass.requiredMethodRef(nme.EQ, List(IntType)).symbol
    @threadUnsafe lazy val Int_>= : Symbol = IntClass.requiredMethodRef(nme.GE, List(IntType)).symbol
    @threadUnsafe lazy val Int_<= : Symbol = IntClass.requiredMethodRef(nme.LE, List(IntType)).symbol
  @threadUnsafe lazy val LongType: TypeRef = valueTypeRef("scala.Long", java.lang.Long.TYPE, LongEnc, nme.specializedTypeNames.Long)
  def LongClass given Context: ClassSymbol = LongType.symbol.asClass
    @threadUnsafe lazy val Long_+ : Symbol = LongClass.requiredMethodRef(nme.PLUS, List(LongType)).symbol
    @threadUnsafe lazy val Long_* : Symbol = LongClass.requiredMethodRef(nme.MUL, List(LongType)).symbol
    @threadUnsafe lazy val Long_/ : Symbol = LongClass.requiredMethodRef(nme.DIV, List(LongType)).symbol

  @threadUnsafe lazy val FloatType: TypeRef = valueTypeRef("scala.Float", java.lang.Float.TYPE, FloatEnc, nme.specializedTypeNames.Float)
  def FloatClass given Context: ClassSymbol = FloatType.symbol.asClass
  @threadUnsafe lazy val DoubleType: TypeRef = valueTypeRef("scala.Double", java.lang.Double.TYPE, DoubleEnc, nme.specializedTypeNames.Double)
  def DoubleClass given Context: ClassSymbol = DoubleType.symbol.asClass

  @threadUnsafe lazy val BoxedUnitClass: ClassSymbol = ctx.requiredClassRef("scala.runtime.BoxedUnit").symbol.asClass
    def BoxedUnit_UNIT given Context: TermSymbol = BoxedUnitClass.linkedClass.requiredValue("UNIT")

  @threadUnsafe lazy val BoxedBooleanClass: ClassSymbol = ctx.requiredClassRef("java.lang.Boolean").symbol.asClass
  @threadUnsafe lazy val BoxedByteClass   : ClassSymbol = ctx.requiredClassRef("java.lang.Byte").symbol.asClass
  @threadUnsafe lazy val BoxedShortClass  : ClassSymbol = ctx.requiredClassRef("java.lang.Short").symbol.asClass
  @threadUnsafe lazy val BoxedCharClass   : ClassSymbol = ctx.requiredClassRef("java.lang.Character").symbol.asClass
  @threadUnsafe lazy val BoxedIntClass    : ClassSymbol = ctx.requiredClassRef("java.lang.Integer").symbol.asClass
  @threadUnsafe lazy val BoxedLongClass   : ClassSymbol = ctx.requiredClassRef("java.lang.Long").symbol.asClass
  @threadUnsafe lazy val BoxedFloatClass  : ClassSymbol = ctx.requiredClassRef("java.lang.Float").symbol.asClass
  @threadUnsafe lazy val BoxedDoubleClass : ClassSymbol = ctx.requiredClassRef("java.lang.Double").symbol.asClass

  @threadUnsafe lazy val BoxedBooleanModule: TermSymbol = ctx.requiredModule("java.lang.Boolean")
  @threadUnsafe lazy val BoxedByteModule   : TermSymbol = ctx.requiredModule("java.lang.Byte")
  @threadUnsafe lazy val BoxedShortModule  : TermSymbol = ctx.requiredModule("java.lang.Short")
  @threadUnsafe lazy val BoxedCharModule   : TermSymbol = ctx.requiredModule("java.lang.Character")
  @threadUnsafe lazy val BoxedIntModule    : TermSymbol = ctx.requiredModule("java.lang.Integer")
  @threadUnsafe lazy val BoxedLongModule   : TermSymbol = ctx.requiredModule("java.lang.Long")
  @threadUnsafe lazy val BoxedFloatModule  : TermSymbol = ctx.requiredModule("java.lang.Float")
  @threadUnsafe lazy val BoxedDoubleModule : TermSymbol = ctx.requiredModule("java.lang.Double")
  @threadUnsafe lazy val BoxedUnitModule   : TermSymbol = ctx.requiredModule("java.lang.Void")

  @threadUnsafe lazy val ByNameParamClass2x: ClassSymbol = enterSpecialPolyClass(tpnme.BYNAME_PARAM_CLASS, Covariant, Seq(AnyType))

  @threadUnsafe lazy val RepeatedParamClass: ClassSymbol = enterSpecialPolyClass(tpnme.REPEATED_PARAM_CLASS, Covariant, Seq(ObjectType, SeqType))

  // fundamental classes
  @threadUnsafe lazy val StringClass: ClassSymbol = ctx.requiredClass("java.lang.String")
  def StringType: Type = StringClass.typeRef
  @threadUnsafe lazy val StringModule: Symbol = StringClass.linkedClass
    @threadUnsafe lazy val String_+ : TermSymbol = enterMethod(StringClass, nme.raw.PLUS, methOfAny(StringType), Final)
    @threadUnsafe lazy val String_valueOf_Object: Symbol = StringModule.info.member(nme.valueOf).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef AnyClass) || (pt isRef ObjectClass)
      case _ => false
    }).symbol

  @threadUnsafe lazy val JavaCloneableClass: ClassSymbol        = ctx.requiredClass("java.lang.Cloneable")
  @threadUnsafe lazy val NullPointerExceptionClass: ClassSymbol = ctx.requiredClass("java.lang.NullPointerException")
  @threadUnsafe lazy val IndexOutOfBoundsException: ClassSymbol = ctx.requiredClass("java.lang.IndexOutOfBoundsException")
  @threadUnsafe lazy val ClassClass: ClassSymbol                = ctx.requiredClass("java.lang.Class")
  @threadUnsafe lazy val BoxedNumberClass: ClassSymbol          = ctx.requiredClass("java.lang.Number")
  @threadUnsafe lazy val ClassCastExceptionClass: ClassSymbol   = ctx.requiredClass("java.lang.ClassCastException")
    @threadUnsafe lazy val ClassCastExceptionClass_stringConstructor: TermSymbol  = ClassCastExceptionClass.info.member(nme.CONSTRUCTOR).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef StringClass)
      case _ => false
    }).symbol.asTerm
  @threadUnsafe lazy val ArithmeticExceptionClass: ClassSymbol  = ctx.requiredClass("java.lang.ArithmeticException")
    @threadUnsafe lazy val ArithmeticExceptionClass_stringConstructor: TermSymbol  = ArithmeticExceptionClass.info.member(nme.CONSTRUCTOR).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef StringClass)
      case _ => false
    }).symbol.asTerm

  @threadUnsafe lazy val JavaSerializableClass: ClassSymbol     = ctx.requiredClass("java.io.Serializable")

  @threadUnsafe lazy val ComparableClass: ClassSymbol           = ctx.requiredClass("java.lang.Comparable")

  @threadUnsafe lazy val SystemClass: ClassSymbol               = ctx.requiredClass("java.lang.System")
  @threadUnsafe lazy val SystemModule: Symbol              = SystemClass.linkedClass

  @threadUnsafe lazy val NoSuchElementExceptionClass = ctx.requiredClass("java.util.NoSuchElementException")
  def NoSuchElementExceptionType = NoSuchElementExceptionClass.typeRef
  @threadUnsafe lazy val IllegalArgumentExceptionClass = ctx.requiredClass("java.lang.IllegalArgumentException")
  def IllegalArgumentExceptionType = IllegalArgumentExceptionClass.typeRef

  // in scalac modified to have Any as parent

  @threadUnsafe lazy val ThrowableType: TypeRef          = ctx.requiredClassRef("java.lang.Throwable")
  def ThrowableClass given Context: ClassSymbol = ThrowableType.symbol.asClass
  @threadUnsafe lazy val SerializableType: TypeRef       =
    if (isNewCollections)
      JavaSerializableClass.typeRef
    else
      ctx.requiredClassRef("scala.Serializable")
  def SerializableClass given Context: ClassSymbol = SerializableType.symbol.asClass

   @threadUnsafe lazy val JavaEnumClass: ClassSymbol = {
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

  @threadUnsafe lazy val StringBuilderClass: ClassSymbol = ctx.requiredClassRef("scala.collection.mutable.StringBuilder").symbol.asClass
  @threadUnsafe lazy val MatchErrorClass   : ClassSymbol = ctx.requiredClassRef("scala.MatchError").symbol.asClass
  @threadUnsafe lazy val ConversionClass   : ClassSymbol = ctx.requiredClass("scala.Conversion").typeRef.symbol.asClass

  @threadUnsafe lazy val StringAddClass    : ClassSymbol = ctx.requiredClassRef("scala.runtime.StringAdd").symbol.asClass
    @threadUnsafe lazy val StringAdd_+ : Symbol = StringAddClass.requiredMethodRef(nme.raw.PLUS).symbol

  @threadUnsafe lazy val StringContextClass: ClassSymbol = ctx.requiredClassRef("scala.StringContext").symbol.asClass
    @threadUnsafe lazy val StringContextS  : Symbol = StringContextClass.requiredMethodRef(nme.s).symbol
    @threadUnsafe lazy val StringContextRaw: Symbol = StringContextClass.requiredMethodRef(nme.raw_).symbol
    @threadUnsafe lazy val StringContext_f : Symbol = StringContextClass.requiredMethodRef(nme.f).symbol
  @threadUnsafe lazy val StringContextModule: Symbol = StringContextClass.companionModule
    @threadUnsafe lazy val StringContextModule_apply: Symbol = StringContextModule.requiredMethodRef(nme.apply).symbol

  @threadUnsafe lazy val InternalStringContextMacroModule: Symbol = ctx.requiredModuleRef("dotty.internal.StringContextMacro").symbol
    @threadUnsafe lazy val InternalStringContextMacroModule_f: Symbol = InternalStringContextMacroModule.requiredMethodRef(nme.f).symbol

  @threadUnsafe lazy val PartialFunctionClass: ClassSymbol = ctx.requiredClassRef("scala.PartialFunction").symbol.asClass
    @threadUnsafe lazy val PartialFunction_isDefinedAt: Symbol = PartialFunctionClass.requiredMethodRef(nme.isDefinedAt).symbol
    @threadUnsafe lazy val PartialFunction_applyOrElse: Symbol = PartialFunctionClass.requiredMethodRef(nme.applyOrElse).symbol

  @threadUnsafe lazy val AbstractPartialFunctionClass: ClassSymbol = ctx.requiredClassRef("scala.runtime.AbstractPartialFunction").symbol.asClass
  @threadUnsafe lazy val FunctionXXLClass: ClassSymbol = ctx.requiredClassRef("scala.FunctionXXL").symbol.asClass
  @threadUnsafe lazy val ScalaSymbolClass: ClassSymbol = ctx.requiredClassRef("scala.Symbol").symbol.asClass
  @threadUnsafe lazy val DynamicClass: ClassSymbol = ctx.requiredClassRef("scala.Dynamic").symbol.asClass
  @threadUnsafe lazy val OptionClass: ClassSymbol = ctx.requiredClassRef("scala.Option").symbol.asClass
  @threadUnsafe lazy val SomeClass: ClassSymbol = ctx.requiredClassRef("scala.Some").symbol.asClass
  @threadUnsafe lazy val NoneModule: Symbol = ctx.requiredModuleRef("scala.None").symbol

  @threadUnsafe lazy val EnumClass: ClassSymbol = ctx.requiredClassRef("scala.Enum").symbol.asClass
    @threadUnsafe lazy val Enum_ordinal: Symbol = EnumClass.requiredMethodRef(nme.ordinal).symbol

  @threadUnsafe lazy val EnumValuesClass: ClassSymbol = ctx.requiredClassRef("scala.runtime.EnumValues").symbol.asClass
  @threadUnsafe lazy val ProductClass: ClassSymbol = ctx.requiredClassRef("scala.Product").symbol.asClass
    @threadUnsafe lazy val Product_canEqual      : Symbol = ProductClass.requiredMethodRef(nme.canEqual_).symbol
    @threadUnsafe lazy val Product_productArity  : Symbol = ProductClass.requiredMethodRef(nme.productArity).symbol
    @threadUnsafe lazy val Product_productElement: Symbol = ProductClass.requiredMethodRef(nme.productElement).symbol
    @threadUnsafe lazy val Product_productPrefix : Symbol = ProductClass.requiredMethodRef(nme.productPrefix).symbol

  @threadUnsafe lazy val IteratorClass: ClassSymbol = ctx.requiredClassRef("scala.collection.Iterator").symbol.asClass
  def IteratorModule(implicit ctx: Context): Symbol = IteratorClass.companionModule

  @threadUnsafe lazy val ModuleSerializationProxyClass: ClassSymbol = ctx.requiredClassRef("scala.runtime.ModuleSerializationProxy").symbol.asClass
    @threadUnsafe lazy val ModuleSerializationProxyConstructor: TermSymbol =
      ModuleSerializationProxyClass.requiredMethod(nme.CONSTRUCTOR, List(ClassType(TypeBounds.empty)))

  @threadUnsafe lazy val MirrorClass: ClassSymbol = ctx.requiredClassRef("scala.deriving.Mirror").symbol.asClass
  @threadUnsafe lazy val Mirror_ProductClass: ClassSymbol = ctx.requiredClassRef("scala.deriving.Mirror.Product").symbol.asClass
    @threadUnsafe lazy val Mirror_Product_fromProduct: Symbol = Mirror_ProductClass.requiredMethodRef(nme.fromProduct).symbol
  @threadUnsafe lazy val Mirror_SumClass: ClassSymbol = ctx.requiredClassRef("scala.deriving.Mirror.Sum").symbol.asClass
  @threadUnsafe lazy val Mirror_SingletonClass: ClassSymbol = ctx.requiredClassRef("scala.deriving.Mirror.Singleton").symbol.asClass
  @threadUnsafe lazy val Mirror_SingletonProxyClass: ClassSymbol = ctx.requiredClassRef("scala.deriving.Mirror.SingletonProxy").symbol.asClass

  @threadUnsafe lazy val LanguageModule: Symbol = ctx.requiredModuleRef("scala.language").symbol
  @threadUnsafe lazy val NonLocalReturnControlClass: ClassSymbol = ctx.requiredClassRef("scala.runtime.NonLocalReturnControl").symbol.asClass
  @threadUnsafe lazy val SelectableClass: ClassSymbol = ctx.requiredClassRef("scala.Selectable").symbol.asClass

  @threadUnsafe lazy val ClassTagClass: ClassSymbol = ctx.requiredClassRef("scala.reflect.ClassTag").symbol.asClass
  @threadUnsafe lazy val ClassTagModule: Symbol = ClassTagClass.companionModule
    @threadUnsafe lazy val ClassTagModule_apply: Symbol = ClassTagModule.requiredMethodRef(nme.apply).symbol

  @threadUnsafe lazy val QuotedExprClass: ClassSymbol = ctx.requiredClassRef("scala.quoted.Expr").symbol.asClass
  @threadUnsafe lazy val QuotedExprModule: Symbol = QuotedExprClass.companionModule

  @threadUnsafe lazy val QuoteContextClass: ClassSymbol = ctx.requiredClassRef("scala.quoted.QuoteContext").symbol.asClass
  @threadUnsafe lazy val QuoteContextModule: Symbol = QuoteContextClass.companionModule
    @threadUnsafe lazy val QuoteContext_macroContext: Symbol = QuoteContextModule.requiredMethodRef("macroContext").symbol

  @threadUnsafe lazy val LiftableModule: Symbol = ctx.requiredModuleRef("scala.quoted.Liftable").symbol

  @threadUnsafe lazy val InternalQuotedModule: Symbol = ctx.requiredModuleRef("scala.internal.Quoted").symbol
    @threadUnsafe lazy val InternalQuoted_exprQuote  : Symbol = InternalQuotedModule.requiredMethodRef("exprQuote").symbol
    @threadUnsafe lazy val InternalQuoted_exprSplice : Symbol = InternalQuotedModule.requiredMethodRef("exprSplice").symbol
    @threadUnsafe lazy val InternalQuoted_typeQuote  : Symbol = InternalQuotedModule.requiredMethodRef("typeQuote").symbol
    @threadUnsafe lazy val InternalQuoted_patternHole: Symbol = InternalQuotedModule.requiredMethodRef("patternHole").symbol
    @threadUnsafe lazy val InternalQuoted_patternBindHoleAnnot: ClassSymbol = InternalQuotedModule.requiredClass("patternBindHole")
    @threadUnsafe lazy val InternalQuoted_QuoteTypeTagAnnot: ClassSymbol = InternalQuotedModule.requiredClass("quoteTypeTag")

  @threadUnsafe lazy val InternalQuotedMatcherModule: Symbol = ctx.requiredModuleRef("scala.internal.quoted.Matcher").symbol
    @threadUnsafe lazy val InternalQuotedMatcher_unapply: Symbol = InternalQuotedMatcherModule.requiredMethodRef(nme.unapply).symbol

  @threadUnsafe lazy val QuotedTypeClass: ClassSymbol = ctx.requiredClassRef("scala.quoted.Type").symbol.asClass
    @threadUnsafe lazy val QuotedType_splice: Symbol = QuotedTypeClass.requiredType(tpnme.splice)

  @threadUnsafe lazy val QuotedTypeModule: Symbol = QuotedTypeClass.companionModule

  @threadUnsafe lazy val QuotedMatchingBindingClass: ClassSymbol = ctx.requiredClassRef("scala.quoted.matching.Bind").symbol.asClass
  @threadUnsafe lazy val TastyReflectionClass: ClassSymbol = ctx.requiredClassRef("scala.tasty.Reflection").symbol.asClass

  @threadUnsafe lazy val Unpickler_unpickleExpr: Symbol = ctx.requiredMethodRef("scala.runtime.quoted.Unpickler.unpickleExpr").symbol
  @threadUnsafe lazy val Unpickler_unpickleType: Symbol = ctx.requiredMethodRef("scala.runtime.quoted.Unpickler.unpickleType").symbol

  @threadUnsafe lazy val EqlClass: ClassSymbol = ctx.requiredClassRef("scala.Eql").symbol.asClass
    def Eql_eqlAny(implicit ctx: Context): TermSymbol = EqlClass.companionModule.requiredMethod(nme.eqlAny)

  @threadUnsafe lazy val TypeBoxClass: ClassSymbol = ctx.requiredClassRef("scala.internal.TypeBox").symbol.asClass
    @threadUnsafe lazy val TypeBox_CAP: TypeSymbol = TypeBoxClass.requiredType(tpnme.CAP)

  @threadUnsafe lazy val MatchCaseClass: ClassSymbol = ctx.requiredClassRef("scala.internal.MatchCase").symbol.asClass
  @threadUnsafe lazy val NotClass: ClassSymbol = ctx.requiredClassRef("scala.implicits.Not").symbol.asClass
    @threadUnsafe lazy val Not_value: Symbol = NotClass.companionModule.requiredMethodRef(nme.value).symbol

  @threadUnsafe lazy val ValueOfClass: ClassSymbol = ctx.requiredClassRef("scala.ValueOf").symbol.asClass
  @threadUnsafe lazy val StatsModule: Symbol = ctx.requiredModuleRef("dotty.tools.dotc.util.Stats").symbol
    @threadUnsafe lazy val Stats_doRecord: Symbol = StatsModule.requiredMethodRef("doRecord").symbol

  @threadUnsafe lazy val XMLTopScopeModule: Symbol = ctx.requiredModuleRef("scala.xml.TopScope").symbol

  @threadUnsafe lazy val CommandLineParserModule: Symbol = ctx.requiredModuleRef("scala.util.CommandLineParser").symbol
    @threadUnsafe lazy val CLP_ParseError: ClassSymbol = CommandLineParserModule.requiredClass("ParseError").typeRef.symbol.asClass
    @threadUnsafe lazy val CLP_parseArgument: Symbol = CommandLineParserModule.requiredMethodRef("parseArgument").symbol
    @threadUnsafe lazy val CLP_parseRemainingArguments: Symbol = CommandLineParserModule.requiredMethodRef("parseRemainingArguments").symbol
    @threadUnsafe lazy val CLP_showError: Symbol = CommandLineParserModule.requiredMethodRef("showError").symbol

  @threadUnsafe lazy val TupleTypeRef: TypeRef = ctx.requiredClassRef("scala.Tuple")
  def TupleClass(implicit ctx: Context): ClassSymbol = TupleTypeRef.symbol.asClass
    @threadUnsafe lazy val Tuple_cons: Symbol = TupleClass.requiredMethodRef("*:").symbol
  @threadUnsafe lazy val NonEmptyTupleTypeRef: TypeRef = ctx.requiredClassRef("scala.NonEmptyTuple")
  def NonEmptyTupleClass(implicit ctx: Context): ClassSymbol = NonEmptyTupleTypeRef.symbol.asClass
    lazy val NonEmptyTuple_tail: Symbol = NonEmptyTupleClass.requiredMethodRef("tail").symbol

  @threadUnsafe lazy val PairClass: ClassSymbol = ctx.requiredClassRef("scala.*:").symbol.asClass
  @threadUnsafe lazy val TupleXXLClass: ClassSymbol = ctx.requiredClassRef("scala.TupleXXL").symbol.asClass
  def TupleXXLModule(implicit ctx: Context): Symbol = TupleXXLClass.companionModule

    def TupleXXL_fromIterator(implicit ctx: Context): Symbol = TupleXXLModule.requiredMethod("fromIterator")

  @threadUnsafe lazy val DynamicTupleModule: Symbol = ctx.requiredModule("scala.runtime.DynamicTuple")
  @threadUnsafe lazy val DynamicTupleModuleClass: Symbol = DynamicTupleModule.moduleClass
    lazy val DynamicTuple_consIterator: Symbol = DynamicTupleModule.requiredMethod("consIterator")
    lazy val DynamicTuple_concatIterator: Symbol = DynamicTupleModule.requiredMethod("concatIterator")
    lazy val DynamicTuple_dynamicApply: Symbol = DynamicTupleModule.requiredMethod("dynamicApply")
    lazy val DynamicTuple_dynamicCons: Symbol = DynamicTupleModule.requiredMethod("dynamicCons")
    lazy val DynamicTuple_dynamicSize: Symbol = DynamicTupleModule.requiredMethod("dynamicSize")
    lazy val DynamicTuple_dynamicTail: Symbol = DynamicTupleModule.requiredMethod("dynamicTail")
    lazy val DynamicTuple_dynamicConcat: Symbol = DynamicTupleModule.requiredMethod("dynamicConcat")
    lazy val DynamicTuple_dynamicToArray: Symbol = DynamicTupleModule.requiredMethod("dynamicToArray")
    lazy val DynamicTuple_productToArray: Symbol = DynamicTupleModule.requiredMethod("productToArray")

  @threadUnsafe lazy val TupledFunctionTypeRef: TypeRef = ctx.requiredClassRef("scala.TupledFunction")
  def TupledFunctionClass(implicit ctx: Context): ClassSymbol = TupledFunctionTypeRef.symbol.asClass

  @threadUnsafe lazy val InternalTupledFunctionTypeRef: TypeRef = ctx.requiredClassRef("scala.internal.TupledFunction")
  def InternalTupleFunctionClass(implicit ctx: Context): ClassSymbol = InternalTupledFunctionTypeRef.symbol.asClass
  def InternalTupleFunctionModule(implicit ctx: Context): Symbol = ctx.requiredModule("scala.internal.TupledFunction")

  // Annotation base classes
  @threadUnsafe lazy val AnnotationClass: ClassSymbol = ctx.requiredClassRef("scala.annotation.Annotation").symbol.asClass
  @threadUnsafe lazy val ClassfileAnnotationClass: ClassSymbol = ctx.requiredClassRef("scala.annotation.ClassfileAnnotation").symbol.asClass
  @threadUnsafe lazy val StaticAnnotationClass: ClassSymbol = ctx.requiredClassRef("scala.annotation.StaticAnnotation").symbol.asClass
  @threadUnsafe lazy val RefiningAnnotationClass: ClassSymbol = ctx.requiredClassRef("scala.annotation.RefiningAnnotation").symbol.asClass

  // Annotation classes
  @threadUnsafe lazy val AliasAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.Alias").symbol.asClass
  @threadUnsafe lazy val AnnotationDefaultAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.AnnotationDefault").symbol.asClass
  @threadUnsafe lazy val BodyAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.Body").symbol.asClass
  @threadUnsafe lazy val ChildAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.Child").symbol.asClass
  @threadUnsafe lazy val WithBoundsAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.WithBounds").symbol.asClass
  @threadUnsafe lazy val CovariantBetweenAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.CovariantBetween").symbol.asClass
  @threadUnsafe lazy val ContravariantBetweenAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.ContravariantBetween").symbol.asClass
  @threadUnsafe lazy val DeprecatedAnnot: ClassSymbol = ctx.requiredClassRef("scala.deprecated").symbol.asClass
  @threadUnsafe lazy val ImplicitAmbiguousAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.implicitAmbiguous").symbol.asClass
  @threadUnsafe lazy val ImplicitNotFoundAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.implicitNotFound").symbol.asClass
  @threadUnsafe lazy val ForceInlineAnnot: ClassSymbol = ctx.requiredClassRef("scala.forceInline").symbol.asClass
  @threadUnsafe lazy val InlineParamAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.InlineParam").symbol.asClass
  @threadUnsafe lazy val InvariantBetweenAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.InvariantBetween").symbol.asClass
  @threadUnsafe lazy val MainAnnot: ClassSymbol = ctx.requiredClassRef("scala.main").symbol.asClass
  @threadUnsafe lazy val MigrationAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.migration").symbol.asClass
  @threadUnsafe lazy val NativeAnnot: ClassSymbol = ctx.requiredClassRef("scala.native").symbol.asClass
  @threadUnsafe lazy val RepeatedAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.Repeated").symbol.asClass
  @threadUnsafe lazy val SourceFileAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.SourceFile").symbol.asClass
  @threadUnsafe lazy val ScalaSignatureAnnot: ClassSymbol = ctx.requiredClassRef("scala.reflect.ScalaSignature").symbol.asClass
  @threadUnsafe lazy val ScalaLongSignatureAnnot: ClassSymbol = ctx.requiredClassRef("scala.reflect.ScalaLongSignature").symbol.asClass
  @threadUnsafe lazy val ScalaStrictFPAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.strictfp").symbol.asClass
  @threadUnsafe lazy val ScalaStaticAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.static").symbol.asClass
  @threadUnsafe lazy val SerialVersionUIDAnnot: ClassSymbol = ctx.requiredClassRef("scala.SerialVersionUID").symbol.asClass
  @threadUnsafe lazy val TASTYSignatureAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.TASTYSignature").symbol.asClass
  @threadUnsafe lazy val TASTYLongSignatureAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.internal.TASTYLongSignature").symbol.asClass
  @threadUnsafe lazy val TailrecAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.tailrec").symbol.asClass
  @threadUnsafe lazy val ThreadUnsafeAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.threadUnsafe").symbol.asClass
  @threadUnsafe lazy val TransientParamAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.constructorOnly").symbol.asClass
  @threadUnsafe lazy val CompileTimeOnlyAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.compileTimeOnly").symbol.asClass
  @threadUnsafe lazy val SwitchAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.switch").symbol.asClass
  @threadUnsafe lazy val ThrowsAnnot: ClassSymbol = ctx.requiredClassRef("scala.throws").symbol.asClass
  @threadUnsafe lazy val TransientAnnot: ClassSymbol = ctx.requiredClassRef("scala.transient").symbol.asClass
  @threadUnsafe lazy val UncheckedAnnot: ClassSymbol = ctx.requiredClassRef("scala.unchecked").symbol.asClass
  @threadUnsafe lazy val UncheckedStableAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.unchecked.uncheckedStable").symbol.asClass
  @threadUnsafe lazy val UncheckedVarianceAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.unchecked.uncheckedVariance").symbol.asClass
  @threadUnsafe lazy val VolatileAnnot: ClassSymbol = ctx.requiredClassRef("scala.volatile").symbol.asClass
  @threadUnsafe lazy val FieldMetaAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.meta.field").symbol.asClass
  @threadUnsafe lazy val GetterMetaAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.meta.getter").symbol.asClass
  @threadUnsafe lazy val SetterMetaAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.meta.setter").symbol.asClass
  @threadUnsafe lazy val ShowAsInfixAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.showAsInfix").symbol.asClass
  @threadUnsafe lazy val FunctionalInterfaceAnnot: ClassSymbol = ctx.requiredClassRef("java.lang.FunctionalInterface").symbol.asClass
  @threadUnsafe lazy val InfixAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.infix").symbol.asClass
  @threadUnsafe lazy val AlphaAnnot: ClassSymbol = ctx.requiredClassRef("scala.annotation.alpha").symbol.asClass

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
    def unapply(pft: Type)(implicit ctx: Context): Option[(Type, List[Type])] = {
      if (pft.isRef(PartialFunctionClass)) {
        val targs = pft.dealias.argInfos
        if (targs.length == 2) Some((targs.head, targs.tail)) else None
      }
      else None
    }
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
   *     Array[_ <: Array[_ <: Number]]
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

  @threadUnsafe lazy val AbstractFunctionType: Array[TypeRef] = mkArityArray("scala.runtime.AbstractFunction", MaxImplementedFunctionArity, 0)
  val AbstractFunctionClassPerRun: PerRun[Array[Symbol]] = new PerRun(implicit ctx => AbstractFunctionType.map(_.symbol.asClass))
  def AbstractFunctionClass(n: Int)(implicit ctx: Context): Symbol = AbstractFunctionClassPerRun()(ctx)(n)
  @threadUnsafe private lazy val ImplementedFunctionType = mkArityArray("scala.Function", MaxImplementedFunctionArity, 0)
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

  @threadUnsafe lazy val TupleType: Array[TypeRef] = mkArityArray("scala.Tuple", MaxTupleArity, 1)

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

    @threadUnsafe lazy val Function0_apply: Symbol = ImplementedFunctionType(0).symbol.requiredMethodRef(nme.apply).symbol

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
    cls == NothingClass || cls == NullClass
  def isBottomType(tp: Type): Boolean =
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

  val StaticRootImportFns: List[() => TermRef] = List[() => TermRef](
    () => JavaLangPackageVal.termRef,
    () => ScalaPackageVal.termRef
  )

  val PredefImportFns: List[() => TermRef] = List[() => TermRef](
    () => ScalaPredefModule.termRef,
    () => DottyPredefModule.termRef
  )

  @threadUnsafe lazy val RootImportFns: List[() => TermRef] =
    if (ctx.settings.YnoImports.value) List.empty[() => TermRef]
    else if (ctx.settings.YnoPredef.value) StaticRootImportFns
    else StaticRootImportFns ++ PredefImportFns

  @threadUnsafe lazy val ShadowableImportNames: Set[TermName] = Set("Predef", "DottyPredef").map(_.toTermName)
  @threadUnsafe lazy val RootImportTypes: List[TermRef] = RootImportFns.map(_())

  /** Modules whose members are in the default namespace and their module classes */
  @threadUnsafe lazy val UnqualifiedOwnerTypes: Set[NamedType] =
    RootImportTypes.toSet[NamedType] ++ RootImportTypes.map(_.symbol.moduleClass.typeRef)

  @threadUnsafe lazy val NotRuntimeClasses: Set[Symbol] = Set(AnyClass, AnyValClass, NullClass, NothingClass)

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
  @threadUnsafe lazy val NoInitClasses: Set[Symbol] = NotRuntimeClasses + FunctionXXLClass

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
  @threadUnsafe lazy val Function1SpecializedParamTypes: collection.Set[TypeRef] =
    Set(IntType, LongType, FloatType, DoubleType)
  @threadUnsafe lazy val Function2SpecializedParamTypes: collection.Set[TypeRef] =
    Set(IntType, LongType, DoubleType)
  @threadUnsafe lazy val Function0SpecializedReturnTypes: collection.Set[TypeRef] =
    ScalaNumericValueTypeList.toSet + UnitType + BooleanType
  @threadUnsafe lazy val Function1SpecializedReturnTypes: collection.Set[TypeRef] =
    Set(UnitType, BooleanType, IntType, FloatType, LongType, DoubleType)
  @threadUnsafe lazy val Function2SpecializedReturnTypes: collection.Set[TypeRef] =
    Function1SpecializedReturnTypes

  @threadUnsafe lazy val Function1SpecializedParamClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function1SpecializedParamTypes.map(_.symbol))
  @threadUnsafe lazy val Function2SpecializedParamClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function2SpecializedParamTypes.map(_.symbol))
  @threadUnsafe lazy val Function0SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function0SpecializedReturnTypes.map(_.symbol))
  @threadUnsafe lazy val Function1SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function1SpecializedReturnTypes.map(_.symbol))
  @threadUnsafe lazy val Function2SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
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
    private[this] var current: RunId = NoRunId
    private[this] var cached: T = _
    def apply()(implicit ctx: Context): T = {
      if (current != ctx.runId) {
        cached = generate(ctx)
        current = ctx.runId
      }
      cached
    }
  }

  @threadUnsafe lazy val ScalaNumericValueTypeList: List[TypeRef] = List(
    ByteType, ShortType, CharType, IntType, LongType, FloatType, DoubleType)

  @threadUnsafe private lazy val ScalaNumericValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypeList.toSet
  @threadUnsafe private lazy val ScalaValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypes + UnitType + BooleanType

  val ScalaNumericValueClasses: PerRun[collection.Set[Symbol]] = new PerRun(implicit ctx => ScalaNumericValueTypes.map(_.symbol))
  val ScalaValueClasses: PerRun[collection.Set[Symbol]]        = new PerRun(implicit ctx => ScalaValueTypes.map(_.symbol))

  val ScalaBoxedClasses: PerRun[collection.Set[Symbol]] = new PerRun(implicit ctx =>
    Set(BoxedByteClass, BoxedShortClass, BoxedCharClass, BoxedIntClass, BoxedLongClass, BoxedFloatClass, BoxedDoubleClass, BoxedUnitClass, BoxedBooleanClass)
  )

  private val valueTypeEnc = mutable.Map[TypeName, PrimitiveClassEnc]()
  private val typeTags = mutable.Map[TypeName, Name]().withDefaultValue(nme.specializedTypeNames.Object)

//  private val unboxedTypeRef = mutable.Map[TypeName, TypeRef]()
//  private val javaTypeToValueTypeRef = mutable.Map[Class[_], TypeRef]()
//  private val valueTypeNamesToJavaType = mutable.Map[TypeName, Class[_]]()

  private def valueTypeRef(name: String, jtype: Class[_], enc: Int, tag: Name): TypeRef = {
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

//  /** The `Class[_]` of a primitive value type name */
//  def valueTypeNameToJavaType(name: TypeName)(implicit ctx: Context): Option[Class[_]] =
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

  @threadUnsafe lazy val specialErasure: SimpleIdentityMap[Symbol, ClassSymbol] =
    SimpleIdentityMap.Empty[Symbol]
      .updated(AnyClass, ObjectClass)
      .updated(AnyValClass, ObjectClass)
      .updated(SingletonClass, ObjectClass)
      .updated(TupleClass, ObjectClass)
      .updated(NonEmptyTupleClass, ProductClass)

  // ----- Initialization ---------------------------------------------------

  /** Lists core classes that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  @threadUnsafe lazy val syntheticScalaClasses: List[TypeSymbol] = List(
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

  @threadUnsafe lazy val syntheticCoreClasses: List[Symbol] = syntheticScalaClasses ++ List(
    EmptyPackageVal,
    OpsPackageClass)

  /** Lists core methods that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  @threadUnsafe lazy val syntheticCoreMethods: List[TermSymbol] =
    AnyMethods ++ ObjectMethods ++ List(String_+, throwMethod)

  @threadUnsafe lazy val reservedScalaClassNames: Set[Name] = syntheticScalaClasses.map(_.name).toSet

  private[this] var isInitialized = false

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
