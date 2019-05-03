package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, SymDenotations._, StdNames._, Names._
import Flags._, Scopes._, Decorators._, NameOps._, Periods._
import unpickleScala2.Scala2Unpickler.ensureConstructor
import scala.collection.mutable
import collection.mutable
import Denotations.SingleDenotation
import util.SimpleIdentityMap

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
  val MaxImplementedFunctionArity: Int = 22
}

/** A class defining symbols and types of standard definitions
 *
 *  Note: There's a much nicer design possible once we have implicit functions.
 *  The idea is explored to some degree in branch wip-definitions (#929): Instead of a type
 *  and a separate symbol definition, we produce in one line an implicit function from
 *  Context to Symbol, and possibly also the corresponding type. This cuts down on all
 *  the duplication encountered here.
 *
 *  wip-definitions tries to do the same with an implicit conversion from a SymbolPerRun
 *  type to a symbol type. The problem with that is universal equality. Comparisons will
 *  not trigger the conversion and will therefore likely return false results.
 *
 *  So the branch is put on hold, until we have implicit functions, which will always
 *  automatically be dereferenced.
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
        val methodType = MethodType.maker(
          isJava = false,
          isImplicit = name.isImplicitFunction,
          isContextual = name.isImplicitFunction,
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
    if (cls.linkedClass.exists) cls.linkedClass.info = NoType
    cls
  }

  lazy val RootClass: ClassSymbol = ctx.newPackageSymbol(
    NoSymbol, nme.ROOT, (root, rootcls) => ctx.base.rootLoader(root)).moduleClass.asClass
  lazy val RootPackage: TermSymbol = ctx.newSymbol(
    NoSymbol, nme.ROOTPKG, PackageCreationFlags, TypeRef(NoPrefix, RootClass))

  lazy val EmptyPackageVal: TermSymbol = ctx.newPackageSymbol(
    RootClass, nme.EMPTY_PACKAGE, (emptypkg, emptycls) => ctx.base.rootLoader(emptypkg)).entered
  lazy val EmptyPackageClass: ClassSymbol = EmptyPackageVal.moduleClass.asClass

  /** A package in which we can place all methods that are interpreted specially by the compiler */
  lazy val OpsPackageVal: TermSymbol = ctx.newCompletePackageSymbol(RootClass, nme.OPS_PACKAGE).entered
  lazy val OpsPackageClass: ClassSymbol = OpsPackageVal.moduleClass.asClass

  lazy val ScalaPackageVal: TermSymbol = ctx.requiredPackage(nme.scala_)
  lazy val ScalaMathPackageVal: TermSymbol = ctx.requiredPackage("scala.math")
  lazy val ScalaPackageClass: ClassSymbol = {
    val cls = ScalaPackageVal.moduleClass.asClass
    cls.info.decls.openForMutations.useSynthesizer(
      name => ctx =>
        if (name.isTypeName && name.isSyntheticFunction) newFunctionNTrait(name.asTypeName)
        else NoSymbol)
    cls
  }
  lazy val ScalaPackageObjectRef: TermRef = ctx.requiredModuleRef("scala.package")
  lazy val JavaPackageVal: TermSymbol = ctx.requiredPackage(nme.java)
  lazy val JavaLangPackageVal: TermSymbol = ctx.requiredPackage(jnme.JavaLang)
  // fundamental modules
  lazy val SysPackage: TermSymbol = ctx.requiredModule("scala.sys.package")
    lazy val Sys_errorR: TermRef = SysPackage.moduleClass.requiredMethodRef(nme.error)
    def Sys_error(implicit ctx: Context): Symbol = Sys_errorR.symbol

  lazy val CompiletimePackageObjectRef: TermRef = ctx.requiredModuleRef("scala.compiletime.package")
  lazy val CompiletimePackageObject: Symbol = CompiletimePackageObjectRef.symbol.moduleClass
    lazy val Compiletime_errorR: TermRef = CompiletimePackageObjectRef.symbol.requiredMethodRef(nme.error)
    def Compiletime_error(implicit ctx: Context): Symbol = Compiletime_errorR.symbol
    lazy val Compiletime_constValueR: TermRef = CompiletimePackageObjectRef.symbol.requiredMethodRef("constValue")
    def Compiletime_constValue(implicit ctx: Context): Symbol = Compiletime_constValueR.symbol
    lazy val Compiletime_constValueOptR: TermRef = CompiletimePackageObjectRef.symbol.requiredMethodRef("constValueOpt")
    def Compiletime_constValueOpt(implicit ctx: Context): Symbol = Compiletime_constValueOptR.symbol

  /** The `scalaShadowing` package is used to safely modify classes and
   *  objects in scala so that they can be used from dotty. They will
   *  be visible as members of the `scala` package, replacing any objects
   *  or classes with the same name. But their binary artifacts are
   *  in `scalaShadowing` so they don't clash with the same-named `scala`
   *  members at runtime.
   */
  lazy val ScalaShadowingPackageVal: TermSymbol = ctx.requiredPackage(nme.scalaShadowing)
  def ScalaShadowingPackageClass(implicit ctx: Context): ClassSymbol = ScalaShadowingPackageVal.moduleClass.asClass

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
  lazy val AnyClass: ClassSymbol = completeClass(enterCompleteClassSymbol(ScalaPackageClass, tpnme.Any, Abstract, Nil), ensureCtor = false)
  def AnyType: TypeRef = AnyClass.typeRef
  lazy val AnyValClass: ClassSymbol = completeClass(enterCompleteClassSymbol(ScalaPackageClass, tpnme.AnyVal, Abstract, List(AnyClass.typeRef)))
  def AnyValType: TypeRef = AnyValClass.typeRef

    lazy val Any_== : TermSymbol       = enterMethod(AnyClass, nme.EQ, methOfAny(BooleanType), Final)
    lazy val Any_!= : TermSymbol       = enterMethod(AnyClass, nme.NE, methOfAny(BooleanType), Final)
    lazy val Any_equals: TermSymbol   = enterMethod(AnyClass, nme.equals_, methOfAny(BooleanType))
    lazy val Any_hashCode: TermSymbol = enterMethod(AnyClass, nme.hashCode_, MethodType(Nil, IntType))
    lazy val Any_toString: TermSymbol = enterMethod(AnyClass, nme.toString_, MethodType(Nil, StringType))
    lazy val Any_## : TermSymbol       = enterMethod(AnyClass, nme.HASHHASH, ExprType(IntType), Final)
    lazy val Any_getClass: TermSymbol = enterMethod(AnyClass, nme.getClass_, MethodType(Nil, ClassClass.typeRef.appliedTo(TypeBounds.empty)), Final)
    lazy val Any_isInstanceOf: TermSymbol = enterT1ParameterlessMethod(AnyClass, nme.isInstanceOf_, _ => BooleanType, Final)
    lazy val Any_asInstanceOf: TermSymbol = enterT1ParameterlessMethod(AnyClass, nme.asInstanceOf_, _.paramRefs(0), Final)
    lazy val Any_typeTest: TermSymbol = enterT1ParameterlessMethod(AnyClass, nme.isInstanceOfPM, _ => BooleanType, Final | Synthetic | Artifact)
    lazy val Any_typeCast: TermSymbol = enterT1ParameterlessMethod(AnyClass, nme.asInstanceOfPM, _.paramRefs(0), Final | Synthetic | Artifact | StableRealizable)
      // generated by pattern matcher, eliminated by erasure

    def AnyMethods: List[TermSymbol] = List(Any_==, Any_!=, Any_equals, Any_hashCode,
      Any_toString, Any_##, Any_getClass, Any_isInstanceOf, Any_asInstanceOf, Any_typeTest, Any_typeCast)

  lazy val ObjectClass: ClassSymbol = {
    val cls = ctx.requiredClass("java.lang.Object")
    assert(!cls.isCompleted, "race for completing java.lang.Object")
    cls.info = ClassInfo(cls.owner.thisType, cls, AnyClass.typeRef :: Nil, newScope)
    cls.setFlag(NoInits)

    // The companion object doesn't really exist, `NoType` is the general
    // technique to do that. Here we need to set it before completing
    // attempt to load Object's classfile, which causes issue #1648.
    val companion = JavaLangPackageVal.info.decl(nme.Object).symbol
    companion.moduleClass.info = NoType // to indicate that it does not really exist
    companion.info = NoType // to indicate that it does not really exist

    completeClass(cls)
  }
  def ObjectType: TypeRef = ObjectClass.typeRef

  lazy val AnyRefAlias: TypeSymbol = enterAliasType(tpnme.AnyRef, ObjectType)
  def AnyRefType: TypeRef = AnyRefAlias.typeRef

    lazy val Object_eq: TermSymbol = enterMethod(ObjectClass, nme.eq, methOfAnyRef(BooleanType), Final)
    lazy val Object_ne: TermSymbol = enterMethod(ObjectClass, nme.ne, methOfAnyRef(BooleanType), Final)
    lazy val Object_synchronized: TermSymbol = enterPolyMethod(ObjectClass, nme.synchronized_, 1,
        pt => MethodType(List(pt.paramRefs(0)), pt.paramRefs(0)), Final)
    lazy val Object_clone: TermSymbol = enterMethod(ObjectClass, nme.clone_, MethodType(Nil, ObjectType), Protected)
    lazy val Object_finalize: TermSymbol = enterMethod(ObjectClass, nme.finalize_, MethodType(Nil, UnitType), Protected)
    lazy val Object_notify: TermSymbol = enterMethod(ObjectClass, nme.notify_, MethodType(Nil, UnitType), Final)
    lazy val Object_notifyAll: TermSymbol = enterMethod(ObjectClass, nme.notifyAll_, MethodType(Nil, UnitType), Final)
    lazy val Object_wait: TermSymbol = enterMethod(ObjectClass, nme.wait_, MethodType(Nil, UnitType), Final)
    lazy val Object_waitL: TermSymbol = enterMethod(ObjectClass, nme.wait_, MethodType(LongType :: Nil, UnitType), Final)
    lazy val Object_waitLI: TermSymbol = enterMethod(ObjectClass, nme.wait_, MethodType(LongType :: IntType :: Nil, UnitType), Final)

    def ObjectMethods: List[TermSymbol] = List(Object_eq, Object_ne, Object_synchronized, Object_clone,
        Object_finalize, Object_notify, Object_notifyAll, Object_wait, Object_waitL, Object_waitLI)

  lazy val AnyKindClass: ClassSymbol = {
    val cls = ctx.newCompleteClassSymbol(ScalaPackageClass, tpnme.AnyKind, AbstractFinal | Permanent, Nil)
    if (!ctx.settings.YnoKindPolymorphism.value) {
      // Enable kind-polymorphism by exposing scala.AnyKind
      cls.entered
    }
    cls
  }
  def AnyKindType: TypeRef = AnyKindClass.typeRef

  lazy val andType: TypeSymbol = enterBinaryAlias(tpnme.AND, AndType(_, _))
  lazy val orType: TypeSymbol = enterBinaryAlias(tpnme.OR, OrType(_, _))

  /** Marker method to indicate an argument to a call-by-name parameter.
   *  Created by byNameClosures and elimByName, eliminated by Erasure,
   */
  lazy val cbnArg: TermSymbol = enterPolyMethod(OpsPackageClass, nme.cbnArg, 1,
      pt => MethodType(List(FunctionOf(Nil, pt.paramRefs(0))), pt.paramRefs(0)))

  /** Method representing a throw */
  lazy val throwMethod: TermSymbol = enterMethod(OpsPackageClass, nme.THROWkw,
      MethodType(List(ThrowableType), NothingType))

  lazy val NothingClass: ClassSymbol = enterCompleteClassSymbol(
    ScalaPackageClass, tpnme.Nothing, AbstractFinal, List(AnyClass.typeRef))
  def NothingType: TypeRef = NothingClass.typeRef
  lazy val RuntimeNothingModuleRef: TermRef = ctx.requiredModuleRef("scala.runtime.Nothing")
  lazy val NullClass: ClassSymbol = enterCompleteClassSymbol(
    ScalaPackageClass, tpnme.Null, AbstractFinal, List(ObjectClass.typeRef))
  def NullType: TypeRef = NullClass.typeRef
  lazy val RuntimeNullModuleRef: TermRef = ctx.requiredModuleRef("scala.runtime.Null")

  lazy val ImplicitScrutineeTypeSym =
    newSymbol(ScalaPackageClass, tpnme.IMPLICITkw, EmptyFlags, TypeBounds.empty).entered
  def ImplicitScrutineeTypeRef: TypeRef = ImplicitScrutineeTypeSym.typeRef


  lazy val ScalaPredefModuleRef: TermRef = ctx.requiredModuleRef("scala.Predef")
  def ScalaPredefModule(implicit ctx: Context): Symbol = ScalaPredefModuleRef.symbol
    lazy val Predef_conformsR: TermRef = ScalaPredefModule.requiredMethodRef(nme.conforms_)
    def Predef_conforms(implicit ctx: Context): Symbol = Predef_conformsR.symbol
    lazy val Predef_classOfR: TermRef = ScalaPredefModule.requiredMethodRef(nme.classOf)
    def Predef_classOf(implicit ctx: Context): Symbol = Predef_classOfR.symbol
    lazy val Predef_undefinedR: TermRef = ScalaPredefModule.requiredMethodRef(nme.???)
    def Predef_undefined(implicit ctx: Context): Symbol = Predef_undefinedR.symbol

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

  lazy val ScalaRuntimeModuleRef: TermRef = ctx.requiredModuleRef("scala.runtime.ScalaRunTime")
  def ScalaRuntimeModule(implicit ctx: Context): Symbol = ScalaRuntimeModuleRef.symbol
  def ScalaRuntimeClass(implicit ctx: Context): ClassSymbol = ScalaRuntimeModule.moduleClass.asClass

    def runtimeMethodRef(name: PreName): TermRef = ScalaRuntimeModule.requiredMethodRef(name)
    def ScalaRuntime_dropR(implicit ctx: Context): TermRef = runtimeMethodRef(nme.drop)
    def ScalaRuntime_drop(implicit ctx: Context): Symbol = ScalaRuntime_dropR.symbol

  lazy val BoxesRunTimeModuleRef: TermRef = ctx.requiredModuleRef("scala.runtime.BoxesRunTime")
  def BoxesRunTimeModule(implicit ctx: Context): Symbol = BoxesRunTimeModuleRef.symbol
  def BoxesRunTimeClass(implicit ctx: Context): ClassSymbol = BoxesRunTimeModule.moduleClass.asClass
  lazy val ScalaStaticsModuleRef: TermRef = ctx.requiredModuleRef("scala.runtime.Statics")
  def ScalaStaticsModule(implicit ctx: Context): Symbol = ScalaStaticsModuleRef.symbol
  def ScalaStaticsClass(implicit ctx: Context): ClassSymbol = ScalaStaticsModule.moduleClass.asClass

    def staticsMethodRef(name: PreName): TermRef = ScalaStaticsModule.requiredMethodRef(name)
    def staticsMethod(name: PreName): TermSymbol = ScalaStaticsModule.requiredMethod(name)

  // Dotty deviation: we cannot use a lazy val here because lazy vals in dotty
  // will return "null" when called recursively, see #1856.
  def DottyPredefModuleRef: TermRef = {
    if (myDottyPredefModuleRef == null) {
      myDottyPredefModuleRef = ctx.requiredModuleRef("dotty.DottyPredef")
      assert(myDottyPredefModuleRef != null)
    }
    myDottyPredefModuleRef
  }
  private[this] var myDottyPredefModuleRef: TermRef = _

  def DottyPredefModule(implicit ctx: Context): Symbol = DottyPredefModuleRef.symbol

  lazy val DottyArraysModuleRef: TermRef = ctx.requiredModuleRef("dotty.runtime.Arrays")
  def DottyArraysModule(implicit ctx: Context): Symbol = DottyArraysModuleRef.symbol
    def newGenericArrayMethod(implicit ctx: Context): TermSymbol = DottyArraysModule.requiredMethod("newGenericArray")
    def newArrayMethod(implicit ctx: Context): TermSymbol = DottyArraysModule.requiredMethod("newArray")

  // TODO: Remove once we drop support for 2.12 standard library
  lazy val isNewCollections: Boolean = ctx.settings.YnewCollections.value

  def getWrapVarargsArrayModule: Symbol = if (isNewCollections) ScalaRuntimeModule else ScalaPredefModule

  // The set of all wrap{X, Ref}Array methods, where X is a value type
  val WrapArrayMethods: PerRun[collection.Set[Symbol]] = new PerRun({ implicit ctx =>
    val methodNames = ScalaValueTypes.map(ast.tpd.wrapArrayMethodName) + nme.wrapRefArray
    methodNames.map(getWrapVarargsArrayModule.requiredMethodRef(_).symbol)
  })

  lazy val NilModuleRef: TermRef = ctx.requiredModuleRef("scala.collection.immutable.Nil")
  def NilModule(implicit ctx: Context): Symbol = NilModuleRef.symbol

  lazy val SingletonClass: ClassSymbol =
    // needed as a synthetic class because Scala 2.x refers to it in classfiles
    // but does not define it as an explicit class.
    enterCompleteClassSymbol(
      ScalaPackageClass, tpnme.Singleton, PureInterfaceCreationFlags | Final,
      List(AnyClass.typeRef), EmptyScope)
  lazy val SingletonType: TypeRef = SingletonClass.typeRef

  lazy val SeqType: TypeRef =
    if (isNewCollections) ctx.requiredClassRef("scala.collection.immutable.Seq")
    else ctx.requiredClassRef("scala.collection.Seq")
  def SeqClass(implicit ctx: Context): ClassSymbol = SeqType.symbol.asClass
    lazy val Seq_applyR: TermRef = SeqClass.requiredMethodRef(nme.apply)
    def Seq_apply(implicit ctx: Context): Symbol = Seq_applyR.symbol
    lazy val Seq_headR: TermRef = SeqClass.requiredMethodRef(nme.head)
    def Seq_head(implicit ctx: Context): Symbol = Seq_headR.symbol
    lazy val Seq_dropR: TermRef = SeqClass.requiredMethodRef(nme.drop)
    def Seq_drop(implicit ctx: Context): Symbol = Seq_dropR.symbol
    lazy val Seq_lengthCompareR: TermRef = SeqClass.requiredMethodRef(nme.lengthCompare, List(IntType))
    def Seq_lengthCompare(implicit ctx: Context): Symbol = Seq_lengthCompareR.symbol
    lazy val Seq_lengthR: TermRef = SeqClass.requiredMethodRef(nme.length)
    def Seq_length(implicit ctx: Context): Symbol = Seq_lengthR.symbol
    lazy val Seq_toSeqR: TermRef = SeqClass.requiredMethodRef(nme.toSeq)
    def Seq_toSeq(implicit ctx: Context): Symbol = Seq_toSeqR.symbol

  lazy val ArrayType: TypeRef = ctx.requiredClassRef("scala.Array")
  def ArrayClass(implicit ctx: Context): ClassSymbol = ArrayType.symbol.asClass
    lazy val Array_applyR: TermRef                 = ArrayClass.requiredMethodRef(nme.apply)
    def Array_apply(implicit ctx: Context): Symbol = Array_applyR.symbol
    lazy val Array_updateR: TermRef                = ArrayClass.requiredMethodRef(nme.update)
    def Array_update(implicit ctx: Context): Symbol = Array_updateR.symbol
    lazy val Array_lengthR: TermRef                = ArrayClass.requiredMethodRef(nme.length)
    def Array_length(implicit ctx: Context): Symbol = Array_lengthR.symbol
    lazy val Array_cloneR: TermRef                 = ArrayClass.requiredMethodRef(nme.clone_)
    def Array_clone(implicit ctx: Context): Symbol = Array_cloneR.symbol
    lazy val ArrayConstructorR: TermRef            = ArrayClass.requiredMethodRef(nme.CONSTRUCTOR)
    def ArrayConstructor(implicit ctx: Context): Symbol = ArrayConstructorR.symbol
  lazy val ArrayModuleType: TermRef = ctx.requiredModuleRef("scala.Array")
  def ArrayModule(implicit ctx: Context): ClassSymbol = ArrayModuleType.symbol.moduleClass.asClass


  lazy val UnitType: TypeRef = valueTypeRef("scala.Unit", BoxedUnitType, java.lang.Void.TYPE, UnitEnc, nme.specializedTypeNames.Void)
  def UnitClass(implicit ctx: Context): ClassSymbol = UnitType.symbol.asClass
  def UnitModuleClass(implicit ctx: Context): Symbol = UnitType.symbol.asClass.linkedClass
  lazy val BooleanType: TypeRef = valueTypeRef("scala.Boolean", BoxedBooleanType, java.lang.Boolean.TYPE, BooleanEnc, nme.specializedTypeNames.Boolean)
  def BooleanClass(implicit ctx: Context): ClassSymbol = BooleanType.symbol.asClass
    lazy val Boolean_notR: TermRef   = BooleanClass.requiredMethodRef(nme.UNARY_!)
    def Boolean_! : Symbol = Boolean_notR.symbol
    lazy val Boolean_andR: TermRef = BooleanClass.requiredMethodRef(nme.ZAND) // ### harmonize required... calls
    def Boolean_&& : Symbol = Boolean_andR.symbol
    lazy val Boolean_orR: TermRef  = BooleanClass.requiredMethodRef(nme.ZOR)
    def Boolean_|| : Symbol = Boolean_orR.symbol
    lazy val Boolean_eqeqR: SingleDenotation = BooleanClass.info.member(nme.EQ).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef BooleanClass)
      case _ => false
    })
    def Boolean_== : Symbol = Boolean_eqeqR.symbol
    lazy val Boolean_neqeqR: SingleDenotation = BooleanClass.info.member(nme.NE).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef BooleanClass)
      case _ => false
    })
    def Boolean_!= : Symbol = Boolean_neqeqR.symbol

  lazy val ByteType: TypeRef = valueTypeRef("scala.Byte", BoxedByteType, java.lang.Byte.TYPE, ByteEnc, nme.specializedTypeNames.Byte)
  def ByteClass(implicit ctx: Context): ClassSymbol = ByteType.symbol.asClass
  lazy val ShortType: TypeRef = valueTypeRef("scala.Short", BoxedShortType, java.lang.Short.TYPE, ShortEnc, nme.specializedTypeNames.Short)
  def ShortClass(implicit ctx: Context): ClassSymbol = ShortType.symbol.asClass
  lazy val CharType: TypeRef = valueTypeRef("scala.Char", BoxedCharType, java.lang.Character.TYPE, CharEnc, nme.specializedTypeNames.Char)
  def CharClass(implicit ctx: Context): ClassSymbol = CharType.symbol.asClass
  lazy val IntType: TypeRef = valueTypeRef("scala.Int", BoxedIntType, java.lang.Integer.TYPE, IntEnc, nme.specializedTypeNames.Int)
  def IntClass(implicit ctx: Context): ClassSymbol = IntType.symbol.asClass
    lazy val Int_minusR: TermRef   = IntClass.requiredMethodRef(nme.MINUS, List(IntType))
    def Int_- : Symbol = Int_minusR.symbol
    lazy val Int_plusR: TermRef   = IntClass.requiredMethodRef(nme.PLUS, List(IntType))
    def Int_+ : Symbol = Int_plusR.symbol
    lazy val Int_divR: TermRef   = IntClass.requiredMethodRef(nme.DIV, List(IntType))
    def Int_/ : Symbol = Int_divR.symbol
    lazy val Int_mulR: TermRef   = IntClass.requiredMethodRef(nme.MUL, List(IntType))
    def Int_* : Symbol = Int_mulR.symbol
    lazy val Int_eqR: TermRef   = IntClass.requiredMethodRef(nme.EQ, List(IntType))
    def Int_== : Symbol = Int_eqR.symbol
    lazy val Int_geR: TermRef   = IntClass.requiredMethodRef(nme.GE, List(IntType))
    def Int_>= : Symbol = Int_geR.symbol
    lazy val Int_leR: TermRef   = IntClass.requiredMethodRef(nme.LE, List(IntType))
    def Int_<= : Symbol = Int_leR.symbol
  lazy val LongType: TypeRef = valueTypeRef("scala.Long", BoxedLongType, java.lang.Long.TYPE, LongEnc, nme.specializedTypeNames.Long)
  def LongClass(implicit ctx: Context): ClassSymbol = LongType.symbol.asClass
    lazy val Long_XOR_Long: Symbol = LongType.member(nme.XOR).requiredSymbol("method", nme.XOR, LongType.denot)(
      x => (x is Method) && (x.info.firstParamTypes.head isRef defn.LongClass)
    )
    lazy val Long_LSR_Int: Symbol = LongType.member(nme.LSR).requiredSymbol("method", nme.LSR, LongType.denot)(
      x => (x is Method) && (x.info.firstParamTypes.head isRef defn.IntClass)
    )
    lazy val Long_plusR: TermRef   = LongClass.requiredMethodRef(nme.PLUS, List(LongType))
    def Long_+ : Symbol = Long_plusR.symbol
    lazy val Long_mulR: TermRef   = LongClass.requiredMethodRef(nme.MUL, List(LongType))
    def Long_* : Symbol = Long_mulR.symbol
    lazy val Long_divR: TermRef   = LongClass.requiredMethodRef(nme.DIV, List(LongType))
    def Long_/ : Symbol = Long_divR.symbol

  lazy val FloatType: TypeRef = valueTypeRef("scala.Float", BoxedFloatType, java.lang.Float.TYPE, FloatEnc, nme.specializedTypeNames.Float)
  def FloatClass(implicit ctx: Context): ClassSymbol = FloatType.symbol.asClass
  lazy val DoubleType: TypeRef = valueTypeRef("scala.Double", BoxedDoubleType, java.lang.Double.TYPE, DoubleEnc, nme.specializedTypeNames.Double)
  def DoubleClass(implicit ctx: Context): ClassSymbol = DoubleType.symbol.asClass

  lazy val BoxedUnitType: TypeRef = ctx.requiredClassRef("scala.runtime.BoxedUnit")
  def BoxedUnitClass(implicit ctx: Context): ClassSymbol = BoxedUnitType.symbol.asClass

    def BoxedUnit_UNIT(implicit ctx: Context): TermSymbol = BoxedUnitClass.linkedClass.requiredValue("UNIT")

  lazy val BoxedBooleanType: TypeRef = ctx.requiredClassRef("java.lang.Boolean")
  def BoxedBooleanClass(implicit ctx: Context): ClassSymbol = BoxedBooleanType.symbol.asClass
  lazy val BoxedByteType: TypeRef = ctx.requiredClassRef("java.lang.Byte")
  def BoxedByteClass(implicit ctx: Context): ClassSymbol = BoxedByteType.symbol.asClass
  lazy val BoxedShortType: TypeRef = ctx.requiredClassRef("java.lang.Short")
  def BoxedShortClass(implicit ctx: Context): ClassSymbol = BoxedShortType.symbol.asClass
  lazy val BoxedCharType: TypeRef = ctx.requiredClassRef("java.lang.Character")
  def BoxedCharClass(implicit ctx: Context): ClassSymbol = BoxedCharType.symbol.asClass
  lazy val BoxedIntType: TypeRef = ctx.requiredClassRef("java.lang.Integer")
  def BoxedIntClass(implicit ctx: Context): ClassSymbol = BoxedIntType.symbol.asClass
  lazy val BoxedLongType: TypeRef = ctx.requiredClassRef("java.lang.Long")
  def BoxedLongClass(implicit ctx: Context): ClassSymbol = BoxedLongType.symbol.asClass
  lazy val BoxedFloatType: TypeRef = ctx.requiredClassRef("java.lang.Float")
  def BoxedFloatClass(implicit ctx: Context): ClassSymbol = BoxedFloatType.symbol.asClass
  lazy val BoxedDoubleType: TypeRef = ctx.requiredClassRef("java.lang.Double")
  def BoxedDoubleClass(implicit ctx: Context): ClassSymbol = BoxedDoubleType.symbol.asClass

  lazy val BoxedBooleanModule: TermSymbol = ctx.requiredModule("java.lang.Boolean")
  lazy val BoxedByteModule: TermSymbol    = ctx.requiredModule("java.lang.Byte")
  lazy val BoxedShortModule: TermSymbol   = ctx.requiredModule("java.lang.Short")
  lazy val BoxedCharModule: TermSymbol    = ctx.requiredModule("java.lang.Character")
  lazy val BoxedIntModule: TermSymbol     = ctx.requiredModule("java.lang.Integer")
  lazy val BoxedLongModule: TermSymbol    = ctx.requiredModule("java.lang.Long")
  lazy val BoxedFloatModule: TermSymbol   = ctx.requiredModule("java.lang.Float")
  lazy val BoxedDoubleModule: TermSymbol  = ctx.requiredModule("java.lang.Double")
  lazy val BoxedUnitModule: TermSymbol    = ctx.requiredModule("java.lang.Void")

  lazy val ByNameParamClass2x: ClassSymbol = enterSpecialPolyClass(tpnme.BYNAME_PARAM_CLASS, Covariant, Seq(AnyType))
  lazy val EqualsPatternClass: ClassSymbol = enterSpecialPolyClass(tpnme.EQUALS_PATTERN, EmptyFlags, Seq(AnyType))

  lazy val RepeatedParamClass: ClassSymbol = enterSpecialPolyClass(tpnme.REPEATED_PARAM_CLASS, Covariant, Seq(ObjectType, SeqType))

  // fundamental classes
  lazy val StringClass: ClassSymbol                = ctx.requiredClass("java.lang.String")
  def StringType: Type                = StringClass.typeRef
  lazy val StringModule: Symbol               = StringClass.linkedClass

    lazy val String_+ : TermSymbol = enterMethod(StringClass, nme.raw.PLUS, methOfAny(StringType), Final)
    lazy val String_valueOf_Object: Symbol = StringModule.info.member(nme.valueOf).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef AnyClass) || (pt isRef ObjectClass)
      case _ => false
    }).symbol

  lazy val JavaCloneableClass: ClassSymbol        = ctx.requiredClass("java.lang.Cloneable")
  lazy val NullPointerExceptionClass: ClassSymbol = ctx.requiredClass("java.lang.NullPointerException")
  lazy val IndexOutOfBoundsException: ClassSymbol = ctx.requiredClass("java.lang.IndexOutOfBoundsException")
  lazy val ClassClass: ClassSymbol                = ctx.requiredClass("java.lang.Class")
  lazy val BoxedNumberClass: ClassSymbol          = ctx.requiredClass("java.lang.Number")
  lazy val ClassCastExceptionClass: ClassSymbol   = ctx.requiredClass("java.lang.ClassCastException")
    lazy val ClassCastExceptionClass_stringConstructor: TermSymbol  = ClassCastExceptionClass.info.member(nme.CONSTRUCTOR).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef StringClass)
      case _ => false
    }).symbol.asTerm
  lazy val ArithmeticExceptionClass: ClassSymbol  = ctx.requiredClass("java.lang.ArithmeticException")
    lazy val ArithmeticExceptionClass_stringConstructor: TermSymbol  = ArithmeticExceptionClass.info.member(nme.CONSTRUCTOR).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef StringClass)
      case _ => false
    }).symbol.asTerm

  lazy val JavaSerializableClass: ClassSymbol     = ctx.requiredClass("java.io.Serializable")

  lazy val ComparableClass: ClassSymbol           = ctx.requiredClass("java.lang.Comparable")

  lazy val SystemClass: ClassSymbol               = ctx.requiredClass("java.lang.System")
  lazy val SystemModule: Symbol              = SystemClass.linkedClass

  // in scalac modified to have Any as parent

  lazy val ThrowableType: TypeRef          = ctx.requiredClassRef("java.lang.Throwable")
  def ThrowableClass(implicit ctx: Context): ClassSymbol = ThrowableType.symbol.asClass
  lazy val SerializableType: TypeRef       =
    if (isNewCollections)
      JavaSerializableClass.typeRef
    else
      ctx.requiredClassRef("scala.Serializable")
  def SerializableClass(implicit ctx: Context): ClassSymbol = SerializableType.symbol.asClass
  lazy val StringBuilderType: TypeRef      = ctx.requiredClassRef("scala.collection.mutable.StringBuilder")
  def StringBuilderClass(implicit ctx: Context): ClassSymbol = StringBuilderType.symbol.asClass
  lazy val MatchErrorType: TypeRef         = ctx.requiredClassRef("scala.MatchError")
  def MatchErrorClass(implicit ctx: Context): ClassSymbol = MatchErrorType.symbol.asClass
  lazy val ConversionType: TypeRef = ctx.requiredClass("scala.Conversion").typeRef
  def ConversionClass(implicit ctx: Context): ClassSymbol = ConversionType.symbol.asClass

  lazy val StringAddType: TypeRef          = ctx.requiredClassRef("scala.runtime.StringAdd")
  def StringAddClass(implicit ctx: Context): ClassSymbol = StringAddType.symbol.asClass

    lazy val StringAdd_plusR: TermRef = StringAddClass.requiredMethodRef(nme.raw.PLUS)
    def StringAdd_+(implicit ctx: Context): Symbol = StringAdd_plusR.symbol

  lazy val StringContextType: TypeRef       = ctx.requiredClassRef("scala.StringContext")
  def StringContextClass(implicit ctx: Context): ClassSymbol = StringContextType.symbol.asClass
    lazy val StringContextSR: TermRef = StringContextClass.requiredMethodRef(nme.s)
    def StringContextS(implicit ctx: Context): Symbol = StringContextSR.symbol
    lazy val StringContextRawR: TermRef = StringContextClass.requiredMethodRef(nme.raw_)
    def StringContextRaw(implicit ctx: Context): Symbol = StringContextRawR.symbol
    lazy val StringContext_fR: TermRef = StringContextClass.requiredMethodRef(nme.f)
    def StringContext_f(implicit ctx: Context): Symbol = StringContext_fR.symbol
  def StringContextModule(implicit ctx: Context): Symbol = StringContextClass.companionModule
    lazy val StringContextModule_applyR: TermRef = StringContextModule.requiredMethodRef(nme.apply)
    def StringContextModule_apply(implicit ctx: Context): Symbol = StringContextModule_applyR.symbol

  lazy val InternalStringContextModuleR: TermRef = ctx.requiredModuleRef("dotty.internal.StringContext")
  def InternalStringContextModule(implicit ctx: Context): Symbol = InternalStringContextModuleR.termSymbol
    lazy val InternalStringContextModule_fR: TermRef = InternalStringContextModule.requiredMethodRef(nme.f)
    def InternalStringContextModule_f(implicit ctx: Context): Symbol = InternalStringContextModule_fR.symbol

  lazy val PartialFunctionType: TypeRef         = ctx.requiredClassRef("scala.PartialFunction")
  def PartialFunctionClass(implicit ctx: Context): ClassSymbol = PartialFunctionType.symbol.asClass
    lazy val PartialFunction_isDefinedAtR: TermRef = PartialFunctionClass.requiredMethodRef(nme.isDefinedAt)
    def PartialFunction_isDefinedAt(implicit ctx: Context): Symbol = PartialFunction_isDefinedAtR.symbol
    lazy val PartialFunction_applyOrElseR: TermRef = PartialFunctionClass.requiredMethodRef(nme.applyOrElse)
    def PartialFunction_applyOrElse(implicit ctx: Context): Symbol = PartialFunction_applyOrElseR.symbol

  lazy val AbstractPartialFunctionType: TypeRef = ctx.requiredClassRef("scala.runtime.AbstractPartialFunction")
  def AbstractPartialFunctionClass(implicit ctx: Context): ClassSymbol = AbstractPartialFunctionType.symbol.asClass

  lazy val FunctionXXLType: TypeRef         = ctx.requiredClassRef("scala.FunctionXXL")
  def FunctionXXLClass(implicit ctx: Context): ClassSymbol = FunctionXXLType.symbol.asClass

  lazy val ScalaSymbolType: TypeRef                    = ctx.requiredClassRef("scala.Symbol")
  def ScalaSymbolClass(implicit ctx: Context): ClassSymbol          = ScalaSymbolType.symbol.asClass
  def ScalaSymbolModule(implicit ctx: Context): Symbol         = ScalaSymbolClass.companionModule
    lazy val ScalaSymbolModule_applyR: TermRef                  = ScalaSymbolModule.requiredMethodRef(nme.apply, List(StringType))
    def ScalaSymbolModule_apply(implicit ctx: Context): Symbol = ScalaSymbolModule_applyR.symbol

  lazy val DynamicType: TypeRef                 = ctx.requiredClassRef("scala.Dynamic")
  def DynamicClass(implicit ctx: Context): ClassSymbol = DynamicType.symbol.asClass
  lazy val OptionType: TypeRef                  = ctx.requiredClassRef("scala.Option")
  def OptionClass(implicit ctx: Context): ClassSymbol = OptionType.symbol.asClass
  lazy val SomeType: TypeRef                  = ctx.requiredClassRef("scala.Some")
  def SomeClass(implicit ctx: Context): ClassSymbol = SomeType.symbol.asClass
  lazy val NoneModuleRef: TermRef                  = ctx.requiredModuleRef("scala.None")
  def NoneClass(implicit ctx: Context): ClassSymbol = NoneModuleRef.symbol.moduleClass.asClass
  lazy val EnumType: TypeRef                    = ctx.requiredClassRef("scala.Enum")
  def EnumClass(implicit ctx: Context): ClassSymbol = EnumType.symbol.asClass
  lazy val EnumValuesType: TypeRef              = ctx.requiredClassRef("scala.runtime.EnumValues")
  def EnumValuesClass(implicit ctx: Context): ClassSymbol = EnumValuesType.symbol.asClass
  lazy val ProductType: TypeRef                 = ctx.requiredClassRef("scala.Product")
  def ProductClass(implicit ctx: Context): ClassSymbol = ProductType.symbol.asClass
    lazy val Product_canEqualR: TermRef = ProductClass.requiredMethodRef(nme.canEqual_)
    def Product_canEqual(implicit ctx: Context): Symbol = Product_canEqualR.symbol
    lazy val Product_productArityR: TermRef = ProductClass.requiredMethodRef(nme.productArity)
    def Product_productArity(implicit ctx: Context): Symbol = Product_productArityR.symbol
    lazy val Product_productElementR: TermRef = ProductClass.requiredMethodRef(nme.productElement)
    def Product_productElement(implicit ctx: Context): Symbol = Product_productElementR.symbol
    lazy val Product_productPrefixR: TermRef = ProductClass.requiredMethodRef(nme.productPrefix)
    def Product_productPrefix(implicit ctx: Context): Symbol = Product_productPrefixR.symbol

  lazy val ModuleSerializationProxyType: TypeRef  = ctx.requiredClassRef("scala.runtime.ModuleSerializationProxy")
  def ModuleSerializationProxyClass(implicit ctx: Context): ClassSymbol = ModuleSerializationProxyType.symbol.asClass
    lazy val ModuleSerializationProxyConstructor: TermSymbol =
      ModuleSerializationProxyClass.requiredMethod(nme.CONSTRUCTOR, List(ClassType(TypeBounds.empty)))

  lazy val GenericType: TypeRef                = ctx.requiredClassRef("scala.reflect.Generic")
  def GenericClass(implicit ctx: Context): ClassSymbol    = GenericType.symbol.asClass
  lazy val ShapeType: TypeRef                  = ctx.requiredClassRef("scala.compiletime.Shape")
  def ShapeClass(implicit ctx: Context): ClassSymbol      = ShapeType.symbol.asClass
  lazy val ShapeCaseType: TypeRef              = ctx.requiredClassRef("scala.compiletime.Shape.Case")
  def ShapeCaseClass(implicit ctx: Context): ClassSymbol  = ShapeCaseType.symbol.asClass
  lazy val ShapeCasesType: TypeRef             = ctx.requiredClassRef("scala.compiletime.Shape.Cases")
  def ShapeCasesClass(implicit ctx: Context): ClassSymbol = ShapeCasesType.symbol.asClass
  lazy val MirrorType: TypeRef                 = ctx.requiredClassRef("scala.reflect.Mirror")
  lazy val GenericClassType: TypeRef           = ctx.requiredClassRef("scala.reflect.GenericClass")

  lazy val LanguageModuleRef: TermSymbol = ctx.requiredModule("scala.language")
  def LanguageModuleClass(implicit ctx: Context): ClassSymbol = LanguageModuleRef.moduleClass.asClass
  lazy val NonLocalReturnControlType: TypeRef   = ctx.requiredClassRef("scala.runtime.NonLocalReturnControl")
  lazy val SelectableType: TypeRef              = ctx.requiredClassRef("scala.Selectable")

  lazy val ClassTagType: TypeRef = ctx.requiredClassRef("scala.reflect.ClassTag")
  def ClassTagClass(implicit ctx: Context): ClassSymbol = ClassTagType.symbol.asClass
  def ClassTagModule(implicit ctx: Context): Symbol = ClassTagClass.companionModule

  lazy val QuotedExprType: TypeRef = ctx.requiredClassRef("scala.quoted.Expr")
  def QuotedExprClass(implicit ctx: Context): ClassSymbol = QuotedExprType.symbol.asClass

  lazy val InternalQuotedModuleRef: TermRef = ctx.requiredModuleRef("scala.internal.Quoted")
  def InternalQuotedModule: Symbol = InternalQuotedModuleRef.symbol
    lazy val InternalQuoted_exprQuoteR: TermRef = InternalQuotedModule.requiredMethodRef("exprQuote")
    def InternalQuoted_exprQuote(implicit ctx: Context): Symbol = InternalQuoted_exprQuoteR.symbol
    lazy val InternalQuoted_exprSpliceR: TermRef = InternalQuotedModule.requiredMethodRef("exprSplice")
    def InternalQuoted_exprSplice(implicit ctx: Context): Symbol = InternalQuoted_exprSpliceR.symbol
    lazy val InternalQuoted_typeQuoteR: TermRef = InternalQuotedModule.requiredMethodRef("typeQuote")
    def InternalQuoted_typeQuote(implicit ctx: Context): Symbol = InternalQuoted_typeQuoteR.symbol
    lazy val InternalQuoted_patternHoleR: TermRef = InternalQuotedModule.requiredMethodRef("patternHole")
    def InternalQuoted_patternHole(implicit ctx: Context): Symbol = InternalQuoted_patternHoleR.symbol
    lazy val InternalQuoted_patternBindHoleAnnot: ClassSymbol = InternalQuotedModule.requiredClass("patternBindHole")

  lazy val InternalQuotedMatcherModuleRef: TermRef = ctx.requiredModuleRef("scala.internal.quoted.Matcher")
    def InternalQuotedMatcherModule(implicit ctx: Context): Symbol = InternalQuotedMatcherModuleRef.symbol

    lazy val InternalQuotedMatcher_unapplyR: TermRef = InternalQuotedMatcherModule.requiredMethodRef(nme.unapply)
    def InternalQuotedMatcher_unapply(implicit ctx: Context) = InternalQuotedMatcher_unapplyR.symbol

  lazy val QuotedExprsModule: TermSymbol = ctx.requiredModule("scala.quoted.Exprs")
  def QuotedExprsClass(implicit ctx: Context): ClassSymbol = QuotedExprsModule.asClass

  lazy val QuotedTypeType: TypeRef = ctx.requiredClassRef("scala.quoted.Type")
  def QuotedTypeClass(implicit ctx: Context): ClassSymbol = QuotedTypeType.symbol.asClass

    lazy val QuotedType_spliceR: TypeRef = QuotedTypeClass.requiredType(tpnme.splice).typeRef
    def QuotedType_splice : Symbol = QuotedType_spliceR.symbol

  lazy val QuotedTypeModuleRef: TermRef = ctx.requiredModuleRef("scala.quoted.Type")
  def QuotedTypeModule(implicit ctx: Context): Symbol = QuotedTypeModuleRef.symbol

  lazy val QuotedMatchingBindingType: TypeRef = ctx.requiredClassRef("scala.quoted.matching.Bind")
  def QuotedMatchingBindingClass(implicit ctx: Context): ClassSymbol = QuotedMatchingBindingType.symbol.asClass

  def Unpickler_unpickleExpr: TermSymbol = ctx.requiredMethod("scala.runtime.quoted.Unpickler.unpickleExpr")
  def Unpickler_liftedExpr: TermSymbol = ctx.requiredMethod("scala.runtime.quoted.Unpickler.liftedExpr")
  def Unpickler_unpickleType: TermSymbol = ctx.requiredMethod("scala.runtime.quoted.Unpickler.unpickleType")

  lazy val TastyReflectionType: TypeRef = ctx.requiredClassRef("scala.tasty.Reflection")
  def TastyReflectionClass(implicit ctx: Context): ClassSymbol = TastyReflectionType.symbol.asClass

  lazy val TastyReflectionModule: TermSymbol = ctx.requiredModule("scala.tasty.Reflection")
    lazy val TastyReflection_macroContext: TermSymbol = TastyReflectionModule.requiredMethod("macroContext")

  lazy val EqlType: TypeRef = ctx.requiredClassRef("scala.Eql")
  def EqlClass(implicit ctx: Context): ClassSymbol = EqlType.symbol.asClass
  def EqlModule(implicit ctx: Context): Symbol = EqlClass.companionModule

    def Eql_eqlAny(implicit ctx: Context): TermSymbol = EqlModule.requiredMethod(nme.eqlAny)

  lazy val TypeBoxType: TypeRef = ctx.requiredClassRef("scala.internal.TypeBox")

    lazy val TypeBox_CAP: TypeSymbol = TypeBoxType.symbol.requiredType(tpnme.CAP)

  lazy val MatchCaseType: TypeRef = ctx.requiredClassRef("scala.internal.MatchCase")
  def MatchCaseClass(implicit ctx: Context): ClassSymbol = MatchCaseType.symbol.asClass

  lazy val NotType: TypeRef = ctx.requiredClassRef("scala.implicits.Not")
  def NotClass(implicit ctx: Context): ClassSymbol = NotType.symbol.asClass
  def NotModule(implicit ctx: Context): Symbol = NotClass.companionModule

    def Not_value(implicit ctx: Context): TermSymbol = NotModule.requiredMethod(nme.value)

  lazy val ValueOfType: TypeRef = ctx.requiredClassRef("scala.ValueOf")
  def ValueOfClass(implicit ctx: Context): ClassSymbol = ValueOfType.symbol.asClass

  lazy val StatsModule = ctx.requiredModule("dotty.tools.dotc.util.Stats")
    def Stats_doRecord(implicit ctx: Context): TermSymbol = StatsModule.requiredMethod("doRecord")

  lazy val XMLTopScopeModuleRef: TermRef = ctx.requiredModuleRef("scala.xml.TopScope")

  lazy val TupleTypeRef: TypeRef = ctx.requiredClassRef("scala.Tuple")
  def TupleClass(implicit ctx: Context): ClassSymbol = TupleTypeRef.symbol.asClass
  lazy val NonEmptyTupleTypeRef: TypeRef = ctx.requiredClassRef("scala.NonEmptyTuple")
  def NonEmptyTupleClass(implicit ctx: Context): ClassSymbol = NonEmptyTupleTypeRef.symbol.asClass

  lazy val PairType: TypeRef = ctx.requiredClassRef("scala.*:")
  def PairClass(implicit ctx: Context): ClassSymbol = PairType.symbol.asClass
  lazy val TupleXXLType: TypeRef = ctx.requiredClassRef("scala.TupleXXL")
  def TupleXXLClass(implicit ctx: Context): ClassSymbol = TupleXXLType.symbol.asClass
  def TupleXXLModule(implicit ctx: Context): Symbol = TupleXXLClass.companionModule

    def TupleXXL_apply(implicit ctx: Context): Symbol =
      TupleXXLModule.info.member(nme.apply).requiredSymbol("method", nme.apply, TupleXXLModule)(_.info.isVarArgsMethod)

  // Annotation base classes
  lazy val AnnotationType: TypeRef              = ctx.requiredClassRef("scala.annotation.Annotation")
  def AnnotationClass(implicit ctx: Context): ClassSymbol = AnnotationType.symbol.asClass
  lazy val ClassfileAnnotationType: TypeRef     = ctx.requiredClassRef("scala.annotation.ClassfileAnnotation")
  def ClassfileAnnotationClass(implicit ctx: Context): ClassSymbol = ClassfileAnnotationType.symbol.asClass
  lazy val StaticAnnotationType: TypeRef        = ctx.requiredClassRef("scala.annotation.StaticAnnotation")
  def StaticAnnotationClass(implicit ctx: Context): ClassSymbol = StaticAnnotationType.symbol.asClass
  lazy val RefiningAnnotationType: TypeRef      = ctx.requiredClassRef("scala.annotation.RefiningAnnotation")
  def RefiningAnnotationClass(implicit ctx: Context): ClassSymbol = RefiningAnnotationType.symbol.asClass

  // Annotation classes
  lazy val AliasAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.Alias")
  def AliasAnnot(implicit ctx: Context): ClassSymbol = AliasAnnotType.symbol.asClass
  lazy val AnnotationDefaultAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.AnnotationDefault")
  def AnnotationDefaultAnnot(implicit ctx: Context): ClassSymbol = AnnotationDefaultAnnotType.symbol.asClass
  lazy val BodyAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.Body")
  def BodyAnnot(implicit ctx: Context): ClassSymbol = BodyAnnotType.symbol.asClass
  lazy val ChildAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.Child")
  def ChildAnnot(implicit ctx: Context): ClassSymbol = ChildAnnotType.symbol.asClass
  lazy val CovariantBetweenAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.CovariantBetween")
  def CovariantBetweenAnnot(implicit ctx: Context): ClassSymbol = CovariantBetweenAnnotType.symbol.asClass
  lazy val ContravariantBetweenAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.ContravariantBetween")
  def ContravariantBetweenAnnot(implicit ctx: Context): ClassSymbol = ContravariantBetweenAnnotType.symbol.asClass
  lazy val DeprecatedAnnotType: TypeRef = ctx.requiredClassRef("scala.deprecated")
  def DeprecatedAnnot(implicit ctx: Context): ClassSymbol = DeprecatedAnnotType.symbol.asClass
  lazy val ImplicitAmbiguousAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.implicitAmbiguous")
  def ImplicitAmbiguousAnnot(implicit ctx: Context): ClassSymbol = ImplicitAmbiguousAnnotType.symbol.asClass
  lazy val ImplicitNotFoundAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.implicitNotFound")
  def ImplicitNotFoundAnnot(implicit ctx: Context): ClassSymbol = ImplicitNotFoundAnnotType.symbol.asClass
  lazy val ForceInlineAnnotType: TypeRef = ctx.requiredClassRef("scala.forceInline")
  def ForceInlineAnnot(implicit ctx: Context): ClassSymbol = ForceInlineAnnotType.symbol.asClass
  lazy val InlineParamAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.InlineParam")
  def InlineParamAnnot(implicit ctx: Context): ClassSymbol = InlineParamAnnotType.symbol.asClass
  lazy val InvariantBetweenAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.InvariantBetween")
  def InvariantBetweenAnnot(implicit ctx: Context): ClassSymbol = InvariantBetweenAnnotType.symbol.asClass
  lazy val MigrationAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.migration")
  def MigrationAnnot(implicit ctx: Context): ClassSymbol = MigrationAnnotType.symbol.asClass
  lazy val NativeAnnotType: TypeRef                   = ctx.requiredClassRef("scala.native")
  def NativeAnnot(implicit ctx: Context): ClassSymbol = NativeAnnotType.symbol.asClass
  lazy val RepeatedAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.Repeated")
  def RepeatedAnnot(implicit ctx: Context): ClassSymbol = RepeatedAnnotType.symbol.asClass
  lazy val SourceFileAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.SourceFile")
  def SourceFileAnnot(implicit ctx: Context): ClassSymbol = SourceFileAnnotType.symbol.asClass
  lazy val ScalaSignatureAnnotType: TypeRef = ctx.requiredClassRef("scala.reflect.ScalaSignature")
  def ScalaSignatureAnnot(implicit ctx: Context): ClassSymbol = ScalaSignatureAnnotType.symbol.asClass
  lazy val ScalaLongSignatureAnnotType: TypeRef = ctx.requiredClassRef("scala.reflect.ScalaLongSignature")
  def ScalaLongSignatureAnnot(implicit ctx: Context): ClassSymbol = ScalaLongSignatureAnnotType.symbol.asClass
  lazy val ScalaStrictFPAnnotType: TypeRef            = ctx.requiredClassRef("scala.annotation.strictfp")
  def ScalaStrictFPAnnot(implicit ctx: Context): ClassSymbol = ScalaStrictFPAnnotType.symbol.asClass
  lazy val ScalaStaticAnnotType: TypeRef            = ctx.requiredClassRef("scala.annotation.static")
  def ScalaStaticAnnot(implicit ctx: Context): ClassSymbol = ScalaStaticAnnotType.symbol.asClass
  lazy val SerialVersionUIDAnnotType: TypeRef         = ctx.requiredClassRef("scala.SerialVersionUID")
  def SerialVersionUIDAnnot(implicit ctx: Context): ClassSymbol = SerialVersionUIDAnnotType.symbol.asClass
  lazy val TASTYSignatureAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.TASTYSignature")
  def TASTYSignatureAnnot(implicit ctx: Context): ClassSymbol = TASTYSignatureAnnotType.symbol.asClass
  lazy val TASTYLongSignatureAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.internal.TASTYLongSignature")
  def TASTYLongSignatureAnnot(implicit ctx: Context): ClassSymbol = TASTYLongSignatureAnnotType.symbol.asClass
  lazy val TailrecAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.tailrec")
  def TailrecAnnot(implicit ctx: Context): ClassSymbol = TailrecAnnotType.symbol.asClass
  lazy val TransientParamAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.constructorOnly")
  def TransientParamAnnot(implicit ctx: Context): ClassSymbol = TransientParamAnnotType.symbol.asClass
  lazy val CompileTimeOnlyAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.compileTimeOnly")
  def CompileTimeOnlyAnnot(implicit ctx: Context): ClassSymbol = CompileTimeOnlyAnnotType.symbol.asClass
  lazy val SwitchAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.switch")
  def SwitchAnnot(implicit ctx: Context): ClassSymbol = SwitchAnnotType.symbol.asClass
  lazy val ThrowsAnnotType: TypeRef = ctx.requiredClassRef("scala.throws")
  def ThrowsAnnot(implicit ctx: Context): ClassSymbol = ThrowsAnnotType.symbol.asClass
  lazy val TransientAnnotType: TypeRef                = ctx.requiredClassRef("scala.transient")
  def TransientAnnot(implicit ctx: Context): ClassSymbol = TransientAnnotType.symbol.asClass
  lazy val UncheckedAnnotType: TypeRef = ctx.requiredClassRef("scala.unchecked")
  def UncheckedAnnot(implicit ctx: Context): ClassSymbol = UncheckedAnnotType.symbol.asClass
  lazy val UncheckedStableAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.unchecked.uncheckedStable")
  def UncheckedStableAnnot(implicit ctx: Context): ClassSymbol = UncheckedStableAnnotType.symbol.asClass
  lazy val UncheckedVarianceAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.unchecked.uncheckedVariance")
  def UncheckedVarianceAnnot(implicit ctx: Context): ClassSymbol = UncheckedVarianceAnnotType.symbol.asClass
  lazy val VolatileAnnotType: TypeRef = ctx.requiredClassRef("scala.volatile")
  def VolatileAnnot(implicit ctx: Context): ClassSymbol = VolatileAnnotType.symbol.asClass
  lazy val FieldMetaAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.meta.field")
  def FieldMetaAnnot(implicit ctx: Context): ClassSymbol = FieldMetaAnnotType.symbol.asClass
  lazy val GetterMetaAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.meta.getter")
  def GetterMetaAnnot(implicit ctx: Context): ClassSymbol = GetterMetaAnnotType.symbol.asClass
  lazy val SetterMetaAnnotType: TypeRef = ctx.requiredClassRef("scala.annotation.meta.setter")
  def SetterMetaAnnot(implicit ctx: Context): ClassSymbol = SetterMetaAnnotType.symbol.asClass
  lazy val ShowAsInfixAnotType: TypeRef = ctx.requiredClassRef("scala.annotation.showAsInfix")
  def ShowAsInfixAnnot(implicit ctx: Context): ClassSymbol = ShowAsInfixAnotType.symbol.asClass
  lazy val FunctionalInterfaceAnnotType = ctx.requiredClassRef("java.lang.FunctionalInterface")
  def FunctionalInterfaceAnnot(implicit ctx: Context) = FunctionalInterfaceAnnotType.symbol.asClass

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
      PartialFunctionType.appliedTo(arg :: result :: Nil)
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
      MatchCaseType.appliedTo(pat, body)
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
    sym.name == tpnme.S && sym.owner == CompiletimePackageObject

  // ----- Symbol sets ---------------------------------------------------

  lazy val AbstractFunctionType: Array[TypeRef] = mkArityArray("scala.runtime.AbstractFunction", MaxImplementedFunctionArity, 0)
  val AbstractFunctionClassPerRun: PerRun[Array[Symbol]] = new PerRun(implicit ctx => AbstractFunctionType.map(_.symbol.asClass))
  def AbstractFunctionClass(n: Int)(implicit ctx: Context): Symbol = AbstractFunctionClassPerRun()(ctx)(n)
  private lazy val ImplementedFunctionType = mkArityArray("scala.Function", MaxImplementedFunctionArity, 0)
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

  lazy val TupleType: Array[TypeRef] = mkArityArray("scala.Tuple", MaxTupleArity, 1)

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

    lazy val Function0_applyR: TermRef = ImplementedFunctionType(0).symbol.requiredMethodRef(nme.apply)
    def Function0_apply(implicit ctx: Context): Symbol = Function0_applyR.symbol

  def FunctionType(n: Int, isContextual: Boolean = false, isErased: Boolean = false)(implicit ctx: Context): TypeRef =
    if (n <= MaxImplementedFunctionArity && (!isContextual || ctx.erasedTypes) && !isErased) ImplementedFunctionType(n)
    else FunctionClass(n, isContextual, isErased).typeRef

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
    else if (arity > 22) FunctionXXLType
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
    () => ScalaPredefModuleRef,
    () => DottyPredefModuleRef
  )

  lazy val RootImportFns: List[() => TermRef] =
    if (ctx.settings.YnoImports.value) List.empty[() => TermRef]
    else if (ctx.settings.YnoPredef.value) StaticRootImportFns
    else StaticRootImportFns ++ PredefImportFns

  lazy val ShadowableImportNames: Set[TermName] = Set("Predef", "DottyPredef").map(_.toTermName)
  lazy val RootImportTypes: List[TermRef] = RootImportFns.map(_())

  /** Modules whose members are in the default namespace and their module classes */
  lazy val UnqualifiedOwnerTypes: Set[NamedType] =
    RootImportTypes.toSet[NamedType] ++ RootImportTypes.map(_.symbol.moduleClass.typeRef)

  lazy val NotRuntimeClasses: Set[Symbol] = Set(AnyClass, AnyValClass, NullClass, NothingClass)

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
  lazy val NoInitClasses: Set[Symbol] = NotRuntimeClasses + FunctionXXLClass

  def isPolymorphicAfterErasure(sym: Symbol): Boolean =
     (sym eq Any_isInstanceOf) || (sym eq Any_asInstanceOf) || (sym eq Object_synchronized)

  def isTupleType(tp: Type)(implicit ctx: Context): Boolean = {
    val arity = tp.dealias.argInfos.length
    arity <= MaxTupleArity && TupleType(arity) != null && (tp isRef TupleType(arity).symbol)
  }

  def tupleType(elems: List[Type]): Type = {
    val arity = elems.length
    if (arity <= MaxTupleArity && TupleType(arity) != null) TupleType(arity).appliedTo(elems)
    else TypeOps.nestedPairs(elems)
  }

  def isProductSubType(tp: Type)(implicit ctx: Context): Boolean =
    tp.derivesFrom(ProductType.symbol)

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
  lazy val Function1SpecializedParamTypes: collection.Set[TypeRef] =
    Set(IntType, LongType, FloatType, DoubleType)
  lazy val Function2SpecializedParamTypes: collection.Set[TypeRef] =
    Set(IntType, LongType, DoubleType)
  lazy val Function0SpecializedReturnTypes: collection.Set[TypeRef] =
    ScalaNumericValueTypeList.toSet + UnitType + BooleanType
  lazy val Function1SpecializedReturnTypes: collection.Set[TypeRef] =
    Set(UnitType, BooleanType, IntType, FloatType, LongType, DoubleType)
  lazy val Function2SpecializedReturnTypes: collection.Set[TypeRef] =
    Function1SpecializedReturnTypes

  lazy val Function1SpecializedParamClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function1SpecializedParamTypes.map(_.symbol))
  lazy val Function2SpecializedParamClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function2SpecializedParamTypes.map(_.symbol))
  lazy val Function0SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function0SpecializedReturnTypes.map(_.symbol))
  lazy val Function1SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
    new PerRun(implicit ctx => Function1SpecializedReturnTypes.map(_.symbol))
  lazy val Function2SpecializedReturnClasses: PerRun[collection.Set[Symbol]] =
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

  lazy val ScalaNumericValueTypeList: List[TypeRef] = List(
    ByteType, ShortType, CharType, IntType, LongType, FloatType, DoubleType)

  private lazy val ScalaNumericValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypeList.toSet
  private lazy val ScalaValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypes + UnitType + BooleanType
  private lazy val ScalaBoxedTypes = ScalaValueTypes map (t => boxedTypes(t.name))

  val ScalaNumericValueClasses: PerRun[collection.Set[Symbol]] = new PerRun(implicit ctx => ScalaNumericValueTypes.map(_.symbol))
  val ScalaValueClasses: PerRun[collection.Set[Symbol]]        = new PerRun(implicit ctx => ScalaValueTypes.map(_.symbol))
  val ScalaBoxedClasses: PerRun[collection.Set[Symbol]]        = new PerRun(implicit ctx => ScalaBoxedTypes.map(_.symbol))

  private val boxedTypes = mutable.Map[TypeName, TypeRef]()
  private val valueTypeEnc = mutable.Map[TypeName, PrimitiveClassEnc]()
  private val typeTags = mutable.Map[TypeName, Name]().withDefaultValue(nme.specializedTypeNames.Object)

//  private val unboxedTypeRef = mutable.Map[TypeName, TypeRef]()
//  private val javaTypeToValueTypeRef = mutable.Map[Class[_], TypeRef]()
//  private val valueTypeNamesToJavaType = mutable.Map[TypeName, Class[_]]()

  private def valueTypeRef(name: String, boxed: TypeRef, jtype: Class[_], enc: Int, tag: Name): TypeRef = {
    val vcls = ctx.requiredClassRef(name)
    boxedTypes(vcls.name) = boxed
    valueTypeEnc(vcls.name) = enc
    typeTags(vcls.name) = tag
//    unboxedTypeRef(boxed.name) = vcls
//    javaTypeToValueTypeRef(jtype) = vcls
//    valueTypeNamesToJavaType(vcls.name) = jtype
    vcls
  }

  /** The type of the boxed class corresponding to primitive value type `tp`. */
  def boxedType(tp: Type)(implicit ctx: Context): TypeRef = boxedTypes(scalaClassName(tp))

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

  lazy val specialErasure: SimpleIdentityMap[Symbol, ClassSymbol] =
    SimpleIdentityMap.Empty[Symbol]
      .updated(AnyClass, ObjectClass)
      .updated(AnyValClass, ObjectClass)
      .updated(SingletonClass, ObjectClass)
      .updated(TupleClass, ObjectClass)
      .updated(NonEmptyTupleClass, ProductClass)

  // ----- Initialization ---------------------------------------------------

  /** Lists core classes that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  lazy val syntheticScalaClasses: List[TypeSymbol] = List(
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
    SingletonClass,
    EqualsPatternClass)

  lazy val syntheticCoreClasses: List[Symbol] = syntheticScalaClasses ++ List(
    EmptyPackageVal,
    OpsPackageClass)

  /** Lists core methods that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  lazy val syntheticCoreMethods: List[TermSymbol] =
    AnyMethods ++ ObjectMethods ++ List(String_+, throwMethod)

  lazy val reservedScalaClassNames: Set[Name] = syntheticScalaClasses.map(_.name).toSet

  private[this] var isInitialized = false

  def init()(implicit ctx: Context): Unit = {
    this.ctx = ctx
    if (!isInitialized) {
      // Enter all symbols from the scalaShadowing package in the scala package
      for (m <- ScalaShadowingPackageClass.info.decls)
        ScalaPackageClass.enter(m)

      // force initialization of every symbol that is synthesized or hijacked by the compiler
      val forced = syntheticCoreClasses ++ syntheticCoreMethods ++ ScalaValueClasses()

      isInitialized = true
    }
  }
}
