package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Denotations._, SymDenotations._, StdNames._, Names._
import Flags._, Scopes._, Decorators._, NameOps._, util.Positions._, Periods._
import unpickleScala2.Scala2Unpickler.ensureConstructor
import scala.annotation.{ switch, meta }
import scala.collection.{ mutable, immutable }
import PartialFunction._
import collection.mutable
import util.common.alwaysZero

object Definitions {

  /** The maximum number of elements in a tuple or product.
   *  This should be removed once we go to hlists.
   */
  val MaxTupleArity = 22

  /** The maximum arity N of a function type that's implemented
   *  as a trait `scala.FunctionN`. Functions of higher arity are possible,
   *  but are mapped in erasure to functions taking a single parameter of type
   *  Object[].
   *  The limit 22 is chosen for Scala2x interop. It could be something
   *  else without affecting the set of programs that can be compiled.
   */
  val MaxImplementedFunctionArity = 22

  /** The maximal arity of a function that can be accessed as member of a structural type */
  val MaxStructuralMethodArity = 7
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
    ctx.newClassSymbol(owner, name, flags | Permanent, infoFn)

  private def enterCompleteClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet, parents: List[TypeRef], decls: Scope = newScope) =
    ctx.newCompleteClassSymbol(owner, name, flags | Permanent, parents, decls).entered

  private def enterTypeField(cls: ClassSymbol, name: TypeName, flags: FlagSet, scope: MutableScope) =
    scope.enter(newSymbol(cls, name, flags, TypeBounds.empty))

  private def enterTypeParam(cls: ClassSymbol, name: TypeName, flags: FlagSet, scope: MutableScope) =
    enterTypeField(cls, name, flags | ClassTypeParamCreationFlags, scope)

  private def enterSyntheticTypeParam(cls: ClassSymbol, paramFlags: FlagSet, scope: MutableScope, suffix: String = "T0") =
    enterTypeParam(cls, suffix.toTypeName.expandedName(cls), paramFlags, scope)

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
        val parentRefs: List[TypeRef] = ctx.normalizeToClassRefs(parents, cls, paramDecls)
        denot.info = ClassInfo(ScalaPackageClass.thisType, cls, parentRefs, paramDecls)
      }
    }
    newClassSymbol(ScalaPackageClass, name, EmptyFlags, completer).entered
  }

  /** The trait FunctionN or ImplicitFunctionN, for some N
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
   *      trait ImplicitFunctionN[T0,...,T{N-1}, R] extends Object with FunctionN[T0,...,T{N-1}, R] {
   *        def apply(implicit $x0: T0, ..., $x{N_1}: T{N-1}): R
   *      }
   */
  def newFunctionNTrait(name: TypeName): ClassSymbol = {
    val completer = new LazyType {
      def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
        val cls = denot.asClass.classSymbol
        val decls = newScope
        val arity = name.functionArity
        val paramNamePrefix = tpnme.scala_ ++ str.NAME_JOIN ++ name ++ str.EXPAND_SEPARATOR
        val argParams =
          for (i <- List.range(1, arity + 1)) yield
            enterTypeParam(cls, paramNamePrefix ++ "T" ++ i.toString, Contravariant, decls)
        val resParam = enterTypeParam(cls, paramNamePrefix ++ "R", Covariant, decls)
        val (methodType, parentTraits) =
          if (name.firstPart.startsWith(str.ImplicitFunction)) {
            val superTrait =
              FunctionType(arity).appliedTo(argParams.map(_.typeRef) ::: resParam.typeRef :: Nil)
            (ImplicitMethodType, ctx.normalizeToClassRefs(superTrait :: Nil, cls, decls))
          }
          else (MethodType, Nil)
        val applyMeth =
          decls.enter(
            newMethod(cls, nme.apply,
              methodType(argParams.map(_.typeRef), resParam.typeRef), Deferred))
        denot.info =
          ClassInfo(ScalaPackageClass.thisType, cls, ObjectType :: parentTraits, decls)
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

  private def enterPolyMethod(cls: ClassSymbol, name: TermName, typeParamCount: Int,
                    resultTypeFn: PolyType => Type, flags: FlagSet = EmptyFlags) = {
    val tparamNames = PolyType.syntheticParamNames(typeParamCount)
    val tparamInfos = tparamNames map (_ => TypeBounds.empty)
    val ptype = PolyType(tparamNames)(_ => tparamInfos, resultTypeFn)
    enterMethod(cls, name, ptype, flags)
  }

  private def enterT1ParameterlessMethod(cls: ClassSymbol, name: TermName, resultTypeFn: PolyType => Type, flags: FlagSet) =
    enterPolyMethod(cls, name, 1, resultTypeFn, flags)

  private def enterT1EmptyParamsMethod(cls: ClassSymbol, name: TermName, resultTypeFn: PolyType => Type, flags: FlagSet) =
    enterPolyMethod(cls, name, 1, pt => MethodType(Nil, resultTypeFn(pt)), flags)

  private def mkArityArray(name: String, arity: Int, countFrom: Int): Array[TypeRef] = {
    val arr = new Array[TypeRef](arity + 1)
    for (i <- countFrom to arity) arr(i) = ctx.requiredClassRef(name + i)
    arr
  }

  private def completeClass(cls: ClassSymbol): ClassSymbol = {
    ensureConstructor(cls, EmptyScope)
    if (cls.linkedClass.exists) cls.linkedClass.info = NoType
    cls
  }

  lazy val RootClass: ClassSymbol = ctx.newPackageSymbol(
    NoSymbol, nme.ROOT, (root, rootcls) => ctx.rootLoader(root)).moduleClass.asClass
  lazy val RootPackage: TermSymbol = ctx.newSymbol(
    NoSymbol, nme.ROOTPKG, PackageCreationFlags, TypeRef(NoPrefix, RootClass))

  lazy val EmptyPackageVal = ctx.newPackageSymbol(
    RootClass, nme.EMPTY_PACKAGE, (emptypkg, emptycls) => ctx.rootLoader(emptypkg)).entered
  lazy val EmptyPackageClass = EmptyPackageVal.moduleClass.asClass

  /** A package in which we can place all methods that are interpreted specially by the compiler */
  lazy val OpsPackageVal = ctx.newCompletePackageSymbol(RootClass, nme.OPS_PACKAGE).entered
  lazy val OpsPackageClass = OpsPackageVal.moduleClass.asClass

  lazy val ScalaPackageVal = ctx.requiredPackage("scala")
  lazy val ScalaMathPackageVal = ctx.requiredPackage("scala.math")
  lazy val ScalaPackageClass = {
    val cls = ScalaPackageVal.moduleClass.asClass
    cls.info.decls.openForMutations.useSynthesizer(
      name => ctx =>
        if (name.isTypeName && name.isSyntheticFunction) newFunctionNTrait(name.asTypeName)
        else NoSymbol)
    cls
  }
  lazy val JavaPackageVal = ctx.requiredPackage("java")
  lazy val JavaLangPackageVal = ctx.requiredPackage("java.lang")
  // fundamental modules
  lazy val SysPackage = ctx.requiredModule("scala.sys.package")
    lazy val Sys_errorR = SysPackage.moduleClass.requiredMethodRef(nme.error)
    def Sys_error(implicit ctx: Context) = Sys_errorR.symbol

  /** The `scalaShadowing` package is used to safely modify classes and
   *  objects in scala so that they can be used from dotty. They will
   *  be visible as members of the `scala` package, replacing any objects
   *  or classes with the same name. But their binary artifacts are
   *  in `scalaShadowing` so they don't clash with the same-named `scala`
   *  members at runtime.
   */
  lazy val ScalaShadowingPackageVal = ctx.requiredPackage(nme.scalaShadowing)
  lazy val ScalaShadowingPackageClass = ScalaShadowingPackageVal.moduleClass.asClass

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
  lazy val AnyClass: ClassSymbol = completeClass(enterCompleteClassSymbol(ScalaPackageClass, tpnme.Any, Abstract, Nil))
  def AnyType = AnyClass.typeRef
  lazy val AnyValClass: ClassSymbol = completeClass(enterCompleteClassSymbol(ScalaPackageClass, tpnme.AnyVal, Abstract, List(AnyClass.typeRef)))
  def AnyValType = AnyValClass.typeRef

    lazy val Any_==       = enterMethod(AnyClass, nme.EQ, methOfAny(BooleanType), Final)
    lazy val Any_!=       = enterMethod(AnyClass, nme.NE, methOfAny(BooleanType), Final)
    lazy val Any_equals   = enterMethod(AnyClass, nme.equals_, methOfAny(BooleanType))
    lazy val Any_hashCode = enterMethod(AnyClass, nme.hashCode_, MethodType(Nil, IntType))
    lazy val Any_toString = enterMethod(AnyClass, nme.toString_, MethodType(Nil, StringType))
    lazy val Any_##       = enterMethod(AnyClass, nme.HASHHASH, ExprType(IntType), Final)
    lazy val Any_getClass = enterMethod(AnyClass, nme.getClass_, MethodType(Nil, ClassClass.typeRef.appliedTo(TypeBounds.empty)), Final)
    lazy val Any_isInstanceOf = enterT1ParameterlessMethod(AnyClass, nme.isInstanceOf_, _ => BooleanType, Final)
    lazy val Any_asInstanceOf = enterT1ParameterlessMethod(AnyClass, nme.asInstanceOf_, TypeParamRef(_, 0), Final)
    lazy val Any_typeTest = enterT1ParameterlessMethod(AnyClass, nme.isInstanceOfPM, _ => BooleanType, Final | Synthetic)
      // generated by pattern matcher, eliminated by erasure

    def AnyMethods = List(Any_==, Any_!=, Any_equals, Any_hashCode,
      Any_toString, Any_##, Any_getClass, Any_isInstanceOf, Any_asInstanceOf, Any_typeTest)

  lazy val ObjectClass: ClassSymbol = {
    val cls = ctx.requiredClass("java.lang.Object")
    assert(!cls.isCompleted, "race for completing java.lang.Object")
    cls.info = ClassInfo(cls.owner.thisType, cls, AnyClass.typeRef :: Nil, newScope)

    // The companion object doesn't really exist, `NoType` is the general
    // technique to do that. Here we need to set it before completing
    // attempt to load Object's classfile, which causes issue #1648.
    val companion = JavaLangPackageVal.info.decl(nme.Object).symbol
    companion.info = NoType // to indicate that it does not really exist

    completeClass(cls)
  }
  def ObjectType = ObjectClass.typeRef

  lazy val AnyRefAlias: TypeSymbol = enterAliasType(tpnme.AnyRef, ObjectType)
  def AnyRefType = AnyRefAlias.typeRef

    lazy val Object_eq = enterMethod(ObjectClass, nme.eq, methOfAnyRef(BooleanType), Final)
    lazy val Object_ne = enterMethod(ObjectClass, nme.ne, methOfAnyRef(BooleanType), Final)
    lazy val Object_synchronized = enterPolyMethod(ObjectClass, nme.synchronized_, 1,
        pt => MethodType(List(TypeParamRef(pt, 0)), TypeParamRef(pt, 0)), Final)
    lazy val Object_clone = enterMethod(ObjectClass, nme.clone_, MethodType(Nil, ObjectType), Protected)
    lazy val Object_finalize = enterMethod(ObjectClass, nme.finalize_, MethodType(Nil, UnitType), Protected)
    lazy val Object_notify = enterMethod(ObjectClass, nme.notify_, MethodType(Nil, UnitType))
    lazy val Object_notifyAll = enterMethod(ObjectClass, nme.notifyAll_, MethodType(Nil, UnitType))
    lazy val Object_wait = enterMethod(ObjectClass, nme.wait_, MethodType(Nil, UnitType))
    lazy val Object_waitL = enterMethod(ObjectClass, nme.wait_, MethodType(LongType :: Nil, UnitType))
    lazy val Object_waitLI = enterMethod(ObjectClass, nme.wait_, MethodType(LongType :: IntType :: Nil, UnitType))

    def ObjectMethods = List(Object_eq, Object_ne, Object_synchronized, Object_clone,
        Object_finalize, Object_notify, Object_notifyAll, Object_wait, Object_waitL, Object_waitLI)

  /** Marker method to indicate an argument to a call-by-name parameter.
   *  Created by byNameClosures and elimByName, eliminated by Erasure,
   */
  lazy val cbnArg = enterPolyMethod(
      OpsPackageClass, nme.cbnArg, 1,
      pt => MethodType(List(FunctionOf(Nil, TypeParamRef(pt, 0))), TypeParamRef(pt, 0)))

  /** Method representing a throw */
  lazy val throwMethod = enterMethod(OpsPackageClass, nme.THROWkw,
      MethodType(List(ThrowableType), NothingType))

  lazy val NothingClass: ClassSymbol = enterCompleteClassSymbol(
    ScalaPackageClass, tpnme.Nothing, AbstractFinal, List(AnyClass.typeRef))
  def NothingType = NothingClass.typeRef
  lazy val NullClass: ClassSymbol = enterCompleteClassSymbol(
    ScalaPackageClass, tpnme.Null, AbstractFinal, List(ObjectClass.typeRef))
  def NullType = NullClass.typeRef

  lazy val ScalaPredefModuleRef = ctx.requiredModuleRef("scala.Predef")
  def ScalaPredefModule(implicit ctx: Context) = ScalaPredefModuleRef.symbol

    lazy val Predef_ConformsR = ScalaPredefModule.requiredClass("<:<").typeRef
    def Predef_Conforms(implicit ctx: Context) = Predef_ConformsR.symbol
    lazy val Predef_conformsR = ScalaPredefModule.requiredMethodRef("$conforms")
    def Predef_conforms(implicit ctx: Context) = Predef_conformsR.symbol
    lazy val Predef_classOfR = ScalaPredefModule.requiredMethodRef("classOf")
    def Predef_classOf(implicit ctx: Context) = Predef_classOfR.symbol
    lazy val Predef_undefinedR = ScalaPredefModule.requiredMethodRef("???")
    def Predef_undefined(implicit ctx: Context) = Predef_undefinedR.symbol

  lazy val ScalaRuntimeModuleRef = ctx.requiredModuleRef("scala.runtime.ScalaRunTime")
  def ScalaRuntimeModule(implicit ctx: Context) = ScalaRuntimeModuleRef.symbol
  def ScalaRuntimeClass(implicit ctx: Context) = ScalaRuntimeModule.moduleClass.asClass

    def runtimeMethodRef(name: PreName) = ScalaRuntimeModule.requiredMethodRef(name)
    def ScalaRuntime_dropR(implicit ctx: Context) = runtimeMethodRef(nme.drop)
    def ScalaRuntime_drop(implicit ctx: Context) = ScalaRuntime_dropR.symbol

  lazy val BoxesRunTimeModuleRef = ctx.requiredModuleRef("scala.runtime.BoxesRunTime")
  def BoxesRunTimeModule(implicit ctx: Context) = BoxesRunTimeModuleRef.symbol
  def BoxesRunTimeClass(implicit ctx: Context) = BoxesRunTimeModule.moduleClass.asClass
  lazy val ScalaStaticsModuleRef = ctx.requiredModuleRef("scala.runtime.Statics")
  def ScalaStaticsModule(implicit ctx: Context) = ScalaStaticsModuleRef.symbol
  def ScalaStaticsClass(implicit ctx: Context) = ScalaStaticsModule.moduleClass.asClass

    def staticsMethodRef(name: PreName) = ScalaStaticsModule.requiredMethodRef(name)
    def staticsMethod(name: PreName) = ScalaStaticsModule.requiredMethod(name)

  // Dotty deviation: we cannot use a lazy val here because lazy vals in dotty
  // will return "null" when called recursively, see #1856.
  def DottyPredefModuleRef = {
    if (myDottyPredefModuleRef == null) {
      myDottyPredefModuleRef = ctx.requiredModuleRef("dotty.DottyPredef")
      assert(myDottyPredefModuleRef != null)
    }
    myDottyPredefModuleRef
  }
  private[this] var myDottyPredefModuleRef: TermRef = _

  def DottyPredefModule(implicit ctx: Context) = DottyPredefModuleRef.symbol

    lazy val Predef_ImplicitConverterR = DottyPredefModule.requiredClass("ImplicitConverter").typeRef
    def Predef_ImplicitConverter(implicit ctx: Context) = Predef_ImplicitConverterR.symbol

  lazy val DottyArraysModuleRef = ctx.requiredModuleRef("dotty.runtime.Arrays")
  def DottyArraysModule(implicit ctx: Context) = DottyArraysModuleRef.symbol
    def newGenericArrayMethod(implicit ctx: Context) = DottyArraysModule.requiredMethod("newGenericArray")
    def newArrayMethod(implicit ctx: Context) = DottyArraysModule.requiredMethod("newArray")

  lazy val NilModuleRef = ctx.requiredModuleRef("scala.collection.immutable.Nil")
  def NilModule(implicit ctx: Context) = NilModuleRef.symbol

  lazy val SingletonClass: ClassSymbol =
    // needed as a synthetic class because Scala 2.x refers to it in classfiles
    // but does not define it as an explicit class.
    enterCompleteClassSymbol(
      ScalaPackageClass, tpnme.Singleton, PureInterfaceCreationFlags | Final,
      List(AnyClass.typeRef), EmptyScope)

  lazy val SeqType: TypeRef = ctx.requiredClassRef("scala.collection.Seq")
  def SeqClass(implicit ctx: Context) = SeqType.symbol.asClass
    lazy val Seq_applyR = SeqClass.requiredMethodRef(nme.apply)
    def Seq_apply(implicit ctx: Context) = Seq_applyR.symbol
    lazy val Seq_headR = SeqClass.requiredMethodRef(nme.head)
    def Seq_head(implicit ctx: Context) = Seq_headR.symbol
    lazy val Seq_dropR = SeqClass.requiredMethodRef(nme.drop)
    def Seq_drop(implicit ctx: Context) = Seq_dropR.symbol
    lazy val Seq_lengthCompareR = SeqClass.requiredMethodRef(nme.lengthCompare)
    def Seq_lengthCompare(implicit ctx: Context) = Seq_lengthCompareR.symbol

  lazy val ArrayType: TypeRef = ctx.requiredClassRef("scala.Array")
  def ArrayClass(implicit ctx: Context) = ArrayType.symbol.asClass
    lazy val Array_applyR                 = ArrayClass.requiredMethodRef(nme.apply)
    def Array_apply(implicit ctx: Context) = Array_applyR.symbol
    lazy val Array_updateR                = ArrayClass.requiredMethodRef(nme.update)
    def Array_update(implicit ctx: Context) = Array_updateR.symbol
    lazy val Array_lengthR                = ArrayClass.requiredMethodRef(nme.length)
    def Array_length(implicit ctx: Context) = Array_lengthR.symbol
    lazy val Array_cloneR                 = ArrayClass.requiredMethodRef(nme.clone_)
    def Array_clone(implicit ctx: Context) = Array_cloneR.symbol
    lazy val ArrayConstructorR            = ArrayClass.requiredMethodRef(nme.CONSTRUCTOR)
    def ArrayConstructor(implicit ctx: Context) = ArrayConstructorR.symbol
  lazy val ArrayModuleType = ctx.requiredModuleRef("scala.Array")
  def ArrayModule(implicit ctx: Context) = ArrayModuleType.symbol.moduleClass.asClass


  lazy val UnitType: TypeRef = valueTypeRef("scala.Unit", BoxedUnitType, java.lang.Void.TYPE, UnitEnc)
  def UnitClass(implicit ctx: Context) = UnitType.symbol.asClass
  lazy val BooleanType = valueTypeRef("scala.Boolean", BoxedBooleanType, java.lang.Boolean.TYPE, BooleanEnc)
  def BooleanClass(implicit ctx: Context) = BooleanType.symbol.asClass
    lazy val Boolean_notR   = BooleanClass.requiredMethodRef(nme.UNARY_!)
    def Boolean_! = Boolean_notR.symbol
    lazy val Boolean_andR = BooleanClass.requiredMethodRef(nme.ZAND) // ### harmonize required... calls
    def Boolean_&& = Boolean_andR.symbol
    lazy val Boolean_orR  = BooleanClass.requiredMethodRef(nme.ZOR)
    def Boolean_|| = Boolean_orR.symbol
    lazy val Boolean_eqeqR = BooleanClass.info.member(nme.EQ).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef BooleanClass)
      case _ => false
    })
    def Boolean_== = Boolean_eqeqR.symbol
    lazy val Boolean_neqeqR = BooleanClass.info.member(nme.NE).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef BooleanClass)
      case _ => false
    })
    def Boolean_!= = Boolean_neqeqR.symbol

  lazy val ByteType: TypeRef = valueTypeRef("scala.Byte", BoxedByteType, java.lang.Byte.TYPE, ByteEnc)
  def ByteClass(implicit ctx: Context) = ByteType.symbol.asClass
  lazy val ShortType: TypeRef = valueTypeRef("scala.Short", BoxedShortType, java.lang.Short.TYPE, ShortEnc)
  def ShortClass(implicit ctx: Context) = ShortType.symbol.asClass
  lazy val CharType: TypeRef = valueTypeRef("scala.Char", BoxedCharType, java.lang.Character.TYPE, CharEnc)
  def CharClass(implicit ctx: Context) = CharType.symbol.asClass
  lazy val IntType: TypeRef = valueTypeRef("scala.Int", BoxedIntType, java.lang.Integer.TYPE, IntEnc)
  def IntClass(implicit ctx: Context) = IntType.symbol.asClass
    lazy val Int_minusR   = IntClass.requiredMethodRef(nme.MINUS, List(IntType))
    def Int_- = Int_minusR.symbol
    lazy val Int_plusR   = IntClass.requiredMethodRef(nme.PLUS, List(IntType))
    def Int_+ = Int_plusR.symbol
    lazy val Int_divR   = IntClass.requiredMethodRef(nme.DIV, List(IntType))
    def Int_/ = Int_divR.symbol
    lazy val Int_mulR   = IntClass.requiredMethodRef(nme.MUL, List(IntType))
    def Int_* = Int_mulR.symbol
    lazy val Int_eqR   = IntClass.requiredMethodRef(nme.EQ, List(IntType))
    def Int_== = Int_eqR.symbol
    lazy val Int_geR   = IntClass.requiredMethodRef(nme.GE, List(IntType))
    def Int_>= = Int_geR.symbol
    lazy val Int_leR   = IntClass.requiredMethodRef(nme.LE, List(IntType))
    def Int_<= = Int_leR.symbol
  lazy val LongType: TypeRef = valueTypeRef("scala.Long", BoxedLongType, java.lang.Long.TYPE, LongEnc)
  def LongClass(implicit ctx: Context) = LongType.symbol.asClass
    lazy val Long_XOR_Long = LongType.member(nme.XOR).requiredSymbol(
      x => (x is Method) && (x.info.firstParamTypes.head isRef defn.LongClass)
    )
    lazy val Long_LSR_Int = LongType.member(nme.LSR).requiredSymbol(
      x => (x is Method) && (x.info.firstParamTypes.head isRef defn.IntClass)
    )
    lazy val Long_plusR   = LongClass.requiredMethodRef(nme.PLUS, List(LongType))
    def Long_+ = Long_plusR.symbol
    lazy val Long_mulR   = LongClass.requiredMethodRef(nme.MUL, List(LongType))
    def Long_* = Long_mulR.symbol
    lazy val Long_divR   = LongClass.requiredMethodRef(nme.DIV, List(LongType))
    def Long_/ = Long_divR.symbol

  lazy val FloatType: TypeRef = valueTypeRef("scala.Float", BoxedFloatType, java.lang.Float.TYPE, FloatEnc)
  def FloatClass(implicit ctx: Context) = FloatType.symbol.asClass
  lazy val DoubleType: TypeRef = valueTypeRef("scala.Double", BoxedDoubleType, java.lang.Double.TYPE, DoubleEnc)
  def DoubleClass(implicit ctx: Context) = DoubleType.symbol.asClass

  lazy val BoxedUnitType: TypeRef = ctx.requiredClassRef("scala.runtime.BoxedUnit")
  def BoxedUnitClass(implicit ctx: Context) = BoxedUnitType.symbol.asClass

    def BoxedUnit_UNIT(implicit ctx: Context) = BoxedUnitClass.linkedClass.requiredValue("UNIT")

  lazy val BoxedBooleanType: TypeRef = ctx.requiredClassRef("java.lang.Boolean")
  def BoxedBooleanClass(implicit ctx: Context) = BoxedBooleanType.symbol.asClass
  lazy val BoxedByteType: TypeRef = ctx.requiredClassRef("java.lang.Byte")
  def BoxedByteClass(implicit ctx: Context) = BoxedByteType.symbol.asClass
  lazy val BoxedShortType: TypeRef = ctx.requiredClassRef("java.lang.Short")
  def BoxedShortClass(implicit ctx: Context) = BoxedShortType.symbol.asClass
  lazy val BoxedCharType: TypeRef = ctx.requiredClassRef("java.lang.Character")
  def BoxedCharClass(implicit ctx: Context) = BoxedCharType.symbol.asClass
  lazy val BoxedIntType: TypeRef = ctx.requiredClassRef("java.lang.Integer")
  def BoxedIntClass(implicit ctx: Context) = BoxedIntType.symbol.asClass
  lazy val BoxedLongType: TypeRef = ctx.requiredClassRef("java.lang.Long")
  def BoxedLongClass(implicit ctx: Context) = BoxedLongType.symbol.asClass
  lazy val BoxedFloatType: TypeRef = ctx.requiredClassRef("java.lang.Float")
  def BoxedFloatClass(implicit ctx: Context) = BoxedFloatType.symbol.asClass
  lazy val BoxedDoubleType: TypeRef = ctx.requiredClassRef("java.lang.Double")
  def BoxedDoubleClass(implicit ctx: Context) = BoxedDoubleType.symbol.asClass

  lazy val BoxedBooleanModule = ctx.requiredModule("java.lang.Boolean")
  lazy val BoxedByteModule    = ctx.requiredModule("java.lang.Byte")
  lazy val BoxedShortModule   = ctx.requiredModule("java.lang.Short")
  lazy val BoxedCharModule    = ctx.requiredModule("java.lang.Character")
  lazy val BoxedIntModule     = ctx.requiredModule("java.lang.Integer")
  lazy val BoxedLongModule    = ctx.requiredModule("java.lang.Long")
  lazy val BoxedFloatModule   = ctx.requiredModule("java.lang.Float")
  lazy val BoxedDoubleModule  = ctx.requiredModule("java.lang.Double")
  lazy val BoxedUnitModule    = ctx.requiredModule("java.lang.Void")

  lazy val ByNameParamClass2x = enterSpecialPolyClass(tpnme.BYNAME_PARAM_CLASS, Covariant, Seq(AnyType))
  lazy val EqualsPatternClass = enterSpecialPolyClass(tpnme.EQUALS_PATTERN, EmptyFlags, Seq(AnyType))

  lazy val RepeatedParamClass = enterSpecialPolyClass(tpnme.REPEATED_PARAM_CLASS, Covariant, Seq(ObjectType, SeqType))

  // fundamental classes
  lazy val StringClass                = ctx.requiredClass("java.lang.String")
  def StringType: Type                = StringClass.typeRef
  lazy val StringModule               = StringClass.linkedClass

    lazy val String_+ = enterMethod(StringClass, nme.raw.PLUS, methOfAny(StringType), Final)
    lazy val String_valueOf_Object = StringModule.info.member(nme.valueOf).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef AnyClass) || (pt isRef ObjectClass)
      case _ => false
    }).symbol

  lazy val JavaCloneableClass        = ctx.requiredClass("java.lang.Cloneable")
  lazy val NullPointerExceptionClass = ctx.requiredClass("java.lang.NullPointerException")
  lazy val IndexOutOfBoundsException = ctx.requiredClass("java.lang.IndexOutOfBoundsException")
  lazy val ClassClass                = ctx.requiredClass("java.lang.Class")
  lazy val BoxedNumberClass          = ctx.requiredClass("java.lang.Number")
  lazy val ThrowableClass            = ctx.requiredClass("java.lang.Throwable")
  lazy val ClassCastExceptionClass   = ctx.requiredClass("java.lang.ClassCastException")
  lazy val ArithmeticExceptionClass  = ctx.requiredClass("java.lang.ArithmeticException")
    lazy val ArithmeticExceptionClass_stringConstructor  = ArithmeticExceptionClass.info.member(nme.CONSTRUCTOR).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef StringClass)
      case _ => false
    }).symbol.asTerm
  lazy val JavaSerializableClass     = ctx.requiredClass("java.io.Serializable")
  lazy val ComparableClass           = ctx.requiredClass("java.lang.Comparable")

  lazy val SystemClass               = ctx.requiredClass("java.lang.System")
  lazy val SystemModule              = SystemClass.linkedClass

  // in scalac modified to have Any as parent

  lazy val SerializableType: TypeRef       = ctx.requiredClassRef("scala.Serializable")
  def SerializableClass(implicit ctx: Context) = SerializableType.symbol.asClass
  lazy val StringBuilderType: TypeRef      = ctx.requiredClassRef("scala.collection.mutable.StringBuilder")
  def StringBuilderClass(implicit ctx: Context) = StringBuilderType.symbol.asClass
  lazy val MatchErrorType: TypeRef         = ctx.requiredClassRef("scala.MatchError")
  def MatchErrorClass(implicit ctx: Context) = MatchErrorType.symbol.asClass

  lazy val StringAddType: TypeRef          = ctx.requiredClassRef("scala.runtime.StringAdd")
  def StringAddClass(implicit ctx: Context) = StringAddType.symbol.asClass

    lazy val StringAdd_plusR = StringAddClass.requiredMethodRef(nme.raw.PLUS)
    def StringAdd_+(implicit ctx: Context) = StringAdd_plusR.symbol

  lazy val PartialFunctionType: TypeRef         = ctx.requiredClassRef("scala.PartialFunction")
  def PartialFunctionClass(implicit ctx: Context) = PartialFunctionType.symbol.asClass
  lazy val AbstractPartialFunctionType: TypeRef = ctx.requiredClassRef("scala.runtime.AbstractPartialFunction")
  def AbstractPartialFunctionClass(implicit ctx: Context) = AbstractPartialFunctionType.symbol.asClass
  lazy val FunctionXXLType: TypeRef         = ctx.requiredClassRef("scala.FunctionXXL")
  def FunctionXXLClass(implicit ctx: Context) = FunctionXXLType.symbol.asClass

  lazy val SymbolType: TypeRef                  = ctx.requiredClassRef("scala.Symbol")
  def SymbolClass(implicit ctx: Context) = SymbolType.symbol.asClass
  lazy val DynamicType: TypeRef                 = ctx.requiredClassRef("scala.Dynamic")
  def DynamicClass(implicit ctx: Context) = DynamicType.symbol.asClass
  lazy val OptionType: TypeRef                  = ctx.requiredClassRef("scala.Option")
  def OptionClass(implicit ctx: Context) = OptionType.symbol.asClass
  lazy val SomeType: TypeRef                  = ctx.requiredClassRef("scala.Some")
  def SomeClass(implicit ctx: Context) = SomeType.symbol.asClass
  lazy val NoneModuleRef: TermRef                  = ctx.requiredModuleRef("scala.None")
  def NoneClass(implicit ctx: Context) = NoneModuleRef.symbol.moduleClass.asClass
  lazy val EnumType: TypeRef                    = ctx.requiredClassRef("scala.Enum")
  def EnumClass(implicit ctx: Context) = EnumType.symbol.asClass
  lazy val EnumValuesType: TypeRef              = ctx.requiredClassRef("scala.runtime.EnumValues")
  def EnumValuesClass(implicit ctx: Context) = EnumValuesType.symbol.asClass
  lazy val ProductType: TypeRef                 = ctx.requiredClassRef("scala.Product")
  def ProductClass(implicit ctx: Context) = ProductType.symbol.asClass
    lazy val Product_canEqualR = ProductClass.requiredMethodRef(nme.canEqual_)
    def Product_canEqual(implicit ctx: Context) = Product_canEqualR.symbol
    lazy val Product_productArityR = ProductClass.requiredMethodRef(nme.productArity)
    def Product_productArity(implicit ctx: Context) = Product_productArityR.symbol
    lazy val Product_productElementR = ProductClass.requiredMethodRef(nme.productElement)
    def Product_productElement(implicit ctx: Context) = Product_productElementR.symbol
    lazy val Product_productPrefixR = ProductClass.requiredMethodRef(nme.productPrefix)
    def Product_productPrefix(implicit ctx: Context) = Product_productPrefixR.symbol
  lazy val LanguageModuleRef = ctx.requiredModule("scala.language")
  def LanguageModuleClass(implicit ctx: Context) = LanguageModuleRef.symbol.moduleClass.asClass
  lazy val NonLocalReturnControlType: TypeRef   = ctx.requiredClassRef("scala.runtime.NonLocalReturnControl")
  lazy val SelectableType: TypeRef              = ctx.requiredClassRef("scala.Selectable")

  lazy val ClassTagType = ctx.requiredClassRef("scala.reflect.ClassTag")
  def ClassTagClass(implicit ctx: Context) = ClassTagType.symbol.asClass
  def ClassTagModule(implicit ctx: Context) = ClassTagClass.companionModule

  lazy val EqType = ctx.requiredClassRef("scala.Eq")
  def EqClass(implicit ctx: Context) = EqType.symbol.asClass
  def EqModule(implicit ctx: Context) = EqClass.companionModule

    def Eq_eqAny(implicit ctx: Context) = EqModule.requiredMethod(nme.eqAny)

  lazy val XMLTopScopeModuleRef = ctx.requiredModuleRef("scala.xml.TopScope")

  // Annotation base classes
  lazy val AnnotationType              = ctx.requiredClassRef("scala.annotation.Annotation")
  def AnnotationClass(implicit ctx: Context) = AnnotationType.symbol.asClass
  lazy val ClassfileAnnotationType     = ctx.requiredClassRef("scala.annotation.ClassfileAnnotation")
  def ClassfileAnnotationClass(implicit ctx: Context) = ClassfileAnnotationType.symbol.asClass
  lazy val StaticAnnotationType        = ctx.requiredClassRef("scala.annotation.StaticAnnotation")
  def StaticAnnotationClass(implicit ctx: Context) = StaticAnnotationType.symbol.asClass

  // Annotation classes
  lazy val AliasAnnotType = ctx.requiredClassRef("scala.annotation.internal.Alias")
  def AliasAnnot(implicit ctx: Context) = AliasAnnotType.symbol.asClass
  lazy val AnnotationDefaultAnnotType = ctx.requiredClassRef("scala.annotation.internal.AnnotationDefault")
  def AnnotationDefaultAnnot(implicit ctx: Context) = AnnotationDefaultAnnotType.symbol.asClass
  lazy val BodyAnnotType = ctx.requiredClassRef("scala.annotation.internal.Body")
  def BodyAnnot(implicit ctx: Context) = BodyAnnotType.symbol.asClass
  lazy val ChildAnnotType = ctx.requiredClassRef("scala.annotation.internal.Child")
  def ChildAnnot(implicit ctx: Context) = ChildAnnotType.symbol.asClass
  lazy val CovariantBetweenAnnotType = ctx.requiredClassRef("scala.annotation.internal.CovariantBetween")
  def CovariantBetweenAnnot(implicit ctx: Context) = CovariantBetweenAnnotType.symbol.asClass
  lazy val ContravariantBetweenAnnotType = ctx.requiredClassRef("scala.annotation.internal.ContravariantBetween")
  def ContravariantBetweenAnnot(implicit ctx: Context) = ContravariantBetweenAnnotType.symbol.asClass
  lazy val DeprecatedAnnotType = ctx.requiredClassRef("scala.deprecated")
  def DeprecatedAnnot(implicit ctx: Context) = DeprecatedAnnotType.symbol.asClass
  lazy val ImplicitNotFoundAnnotType = ctx.requiredClassRef("scala.annotation.implicitNotFound")
  def ImplicitNotFoundAnnot(implicit ctx: Context) = ImplicitNotFoundAnnotType.symbol.asClass
  lazy val InlineAnnotType = ctx.requiredClassRef("scala.inline")
  def InlineAnnot(implicit ctx: Context) = InlineAnnotType.symbol.asClass
  lazy val InlineParamAnnotType = ctx.requiredClassRef("scala.annotation.internal.InlineParam")
  def InlineParamAnnot(implicit ctx: Context) = InlineParamAnnotType.symbol.asClass
  lazy val InvariantBetweenAnnotType = ctx.requiredClassRef("scala.annotation.internal.InvariantBetween")
  def InvariantBetweenAnnot(implicit ctx: Context) = InvariantBetweenAnnotType.symbol.asClass
  lazy val MigrationAnnotType = ctx.requiredClassRef("scala.annotation.migration")
  def MigrationAnnot(implicit ctx: Context) = MigrationAnnotType.symbol.asClass
  lazy val NativeAnnotType                   = ctx.requiredClassRef("scala.native")
  def NativeAnnot(implicit ctx: Context) = NativeAnnotType.symbol.asClass
  lazy val RemoteAnnotType                   = ctx.requiredClassRef("scala.remote")
  def RemoteAnnot(implicit ctx: Context) = RemoteAnnotType.symbol.asClass
  lazy val RepeatedAnnotType = ctx.requiredClassRef("scala.annotation.internal.Repeated")
  def RepeatedAnnot(implicit ctx: Context) = RepeatedAnnotType.symbol.asClass
  lazy val SourceFileAnnotType = ctx.requiredClassRef("scala.annotation.internal.SourceFile")
  def SourceFileAnnot(implicit ctx: Context) = SourceFileAnnotType.symbol.asClass
  lazy val ScalaSignatureAnnotType = ctx.requiredClassRef("scala.reflect.ScalaSignature")
  def ScalaSignatureAnnot(implicit ctx: Context) = ScalaSignatureAnnotType.symbol.asClass
  lazy val ScalaLongSignatureAnnotType = ctx.requiredClassRef("scala.reflect.ScalaLongSignature")
  def ScalaLongSignatureAnnot(implicit ctx: Context) = ScalaLongSignatureAnnotType.symbol.asClass
  lazy val ScalaStrictFPAnnotType            = ctx.requiredClassRef("scala.annotation.strictfp")
  def ScalaStrictFPAnnot(implicit ctx: Context) = ScalaStrictFPAnnotType.symbol.asClass
  lazy val ScalaStaticAnnotType            = ctx.requiredClassRef("scala.annotation.static")
  def ScalaStaticAnnot(implicit ctx: Context) = ScalaStaticAnnotType.symbol.asClass
  lazy val SerialVersionUIDAnnotType         = ctx.requiredClassRef("scala.SerialVersionUID")
  def SerialVersionUIDAnnot(implicit ctx: Context) = SerialVersionUIDAnnotType.symbol.asClass
  lazy val TASTYSignatureAnnotType = ctx.requiredClassRef("scala.annotation.internal.TASTYSignature")
  def TASTYSignatureAnnot(implicit ctx: Context) = TASTYSignatureAnnotType.symbol.asClass
  lazy val TASTYLongSignatureAnnotType = ctx.requiredClassRef("scala.annotation.internal.TASTYLongSignature")
  def TASTYLongSignatureAnnot(implicit ctx: Context) = TASTYLongSignatureAnnotType.symbol.asClass
  lazy val TailrecAnnotType = ctx.requiredClassRef("scala.annotation.tailrec")
  def TailrecAnnot(implicit ctx: Context) = TailrecAnnotType.symbol.asClass
  lazy val SwitchAnnotType = ctx.requiredClassRef("scala.annotation.switch")
  def SwitchAnnot(implicit ctx: Context) = SwitchAnnotType.symbol.asClass
  lazy val ThrowsAnnotType = ctx.requiredClassRef("scala.throws")
  def ThrowsAnnot(implicit ctx: Context) = ThrowsAnnotType.symbol.asClass
  lazy val TransientAnnotType                = ctx.requiredClassRef("scala.transient")
  def TransientAnnot(implicit ctx: Context) = TransientAnnotType.symbol.asClass
  lazy val UncheckedAnnotType = ctx.requiredClassRef("scala.unchecked")
  def UncheckedAnnot(implicit ctx: Context) = UncheckedAnnotType.symbol.asClass
  lazy val UncheckedStableAnnotType = ctx.requiredClassRef("scala.annotation.unchecked.uncheckedStable")
  def UncheckedStableAnnot(implicit ctx: Context) = UncheckedStableAnnotType.symbol.asClass
  lazy val UncheckedVarianceAnnotType = ctx.requiredClassRef("scala.annotation.unchecked.uncheckedVariance")
  def UncheckedVarianceAnnot(implicit ctx: Context) = UncheckedVarianceAnnotType.symbol.asClass
  lazy val VolatileAnnotType = ctx.requiredClassRef("scala.volatile")
  def VolatileAnnot(implicit ctx: Context) = VolatileAnnotType.symbol.asClass
  lazy val FieldMetaAnnotType = ctx.requiredClassRef("scala.annotation.meta.field")
  def FieldMetaAnnot(implicit ctx: Context) = FieldMetaAnnotType.symbol.asClass
  lazy val GetterMetaAnnotType = ctx.requiredClassRef("scala.annotation.meta.getter")
  def GetterMetaAnnot(implicit ctx: Context) = GetterMetaAnnotType.symbol.asClass
  lazy val SetterMetaAnnotType = ctx.requiredClassRef("scala.annotation.meta.setter")
  def SetterMetaAnnot(implicit ctx: Context) = SetterMetaAnnotType.symbol.asClass

  // convenient one-parameter method types
  def methOfAny(tp: Type) = MethodType(List(AnyType), tp)
  def methOfAnyVal(tp: Type) = MethodType(List(AnyValType), tp)
  def methOfAnyRef(tp: Type) = MethodType(List(ObjectType), tp)

  // Derived types

  def RepeatedParamType = RepeatedParamClass.typeRef
  def ThrowableType = ThrowableClass.typeRef

  def ClassType(arg: Type)(implicit ctx: Context) = {
    val ctype = ClassClass.typeRef
    if (ctx.phase.erasedTypes) ctype else ctype.appliedTo(arg)
  }

  /** The enumeration type, goven a value of the enumeration */
  def EnumType(sym: Symbol)(implicit ctx: Context) =
    // given (in java): "class A { enum E { VAL1 } }"
    //  - sym: the symbol of the actual enumeration value (VAL1)
    //  - .owner: the ModuleClassSymbol of the enumeration (object E)
    //  - .linkedClass: the ClassSymbol of the enumeration (class E)
    sym.owner.linkedClass.typeRef

  object FunctionOf {
    def apply(args: List[Type], resultType: Type, isImplicit: Boolean = false)(implicit ctx: Context) =
      FunctionType(args.length, isImplicit).appliedTo(args ::: resultType :: Nil)
    def unapply(ft: Type)(implicit ctx: Context) = {
      val tsym = ft.typeSymbol
      if (isFunctionClass(tsym)) {
        val targs = ft.dealias.argInfos
        Some(targs.init, targs.last, tsym.name.isImplicitFunction)
      }
      else None
    }
  }

  object PartialFunctionOf {
    def apply(arg: Type, result: Type)(implicit ctx: Context) =
      PartialFunctionType.appliedTo(arg :: result :: Nil)
    def unapply(pft: Type)(implicit ctx: Context) = {
      if (pft.isRef(PartialFunctionClass)) {
        val targs = pft.dealias.argInfos
        if (targs.length == 2) Some((targs.head, targs.tail)) else None
      }
      else None
    }
  }

  object ArrayOf {
    def apply(elem: Type)(implicit ctx: Context) =
      if (ctx.erasedTypes) JavaArrayType(elem)
      else ArrayType.appliedTo(elem :: Nil)
    def unapply(tp: Type)(implicit ctx: Context): Option[Type] = tp.dealias match {
      case at: RefinedType if (at isRef ArrayType.symbol) && at.argInfos.length == 1 => Some(at.argInfos.head)
      case _ => None
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

  // ----- Symbol sets ---------------------------------------------------

  lazy val AbstractFunctionType = mkArityArray("scala.runtime.AbstractFunction", MaxImplementedFunctionArity, 0)
  val AbstractFunctionClassPerRun = new PerRun[Array[Symbol]](implicit ctx => AbstractFunctionType.map(_.symbol.asClass))
  def AbstractFunctionClass(n: Int)(implicit ctx: Context) = AbstractFunctionClassPerRun()(ctx)(n)
  private lazy val ImplementedFunctionType = mkArityArray("scala.Function", MaxImplementedFunctionArity, 0)
  def FunctionClassPerRun = new PerRun[Array[Symbol]](implicit ctx => ImplementedFunctionType.map(_.symbol.asClass))

  lazy val TupleType = mkArityArray("scala.Tuple", MaxTupleArity, 2)

  def FunctionClass(n: Int, isImplicit: Boolean = false)(implicit ctx: Context) =
    if (isImplicit) {
      require(n > 0)
      ctx.requiredClass("scala.ImplicitFunction" + n.toString)
    }
    else if (n <= MaxImplementedFunctionArity) FunctionClassPerRun()(ctx)(n)
    else ctx.requiredClass("scala.Function" + n.toString)

    lazy val Function0_applyR = ImplementedFunctionType(0).symbol.requiredMethodRef(nme.apply)
    def Function0_apply(implicit ctx: Context) = Function0_applyR.symbol

  def FunctionType(n: Int, isImplicit: Boolean = false)(implicit ctx: Context): TypeRef =
    if (n <= MaxImplementedFunctionArity && (!isImplicit || ctx.erasedTypes)) ImplementedFunctionType(n)
    else FunctionClass(n, isImplicit).typeRef

  private lazy val TupleTypes: Set[TypeRef] = TupleType.toSet

  /** If `cls` is a class in the scala package, its name, otherwise EmptyTypeName */
  def scalaClassName(cls: Symbol)(implicit ctx: Context): TypeName =
    if (cls.isClass && cls.owner == ScalaPackageClass) cls.asClass.name else EmptyTypeName

  /** If type `ref` refers to a class in the scala package, its name, otherwise EmptyTypeName */
  def scalaClassName(ref: Type)(implicit ctx: Context): TypeName = scalaClassName(ref.classSymbol)

  private def isVarArityClass(cls: Symbol, prefix: String) =
    scalaClassName(cls).testSimple(name =>
      name.startsWith(prefix) &&
      name.length > prefix.length &&
      name.drop(prefix.length).forall(_.isDigit))

  def isBottomClass(cls: Symbol) =
    cls == NothingClass || cls == NullClass
  def isBottomType(tp: Type) =
    tp.derivesFrom(NothingClass) || tp.derivesFrom(NullClass)

  /** Is a function class.
   *   - FunctionN for N >= 0
   *   - ImplicitFunctionN for N > 0
   */
  def isFunctionClass(cls: Symbol) = scalaClassName(cls).isFunction

  /** Is an implicit function class.
   *   - ImplicitFunctionN for N > 0
   */
  def isImplicitFunctionClass(cls: Symbol) = scalaClassName(cls).isImplicitFunction

  /** Is a class that will be erased to FunctionXXL
   *   - FunctionN for N >= 22
   *   - ImplicitFunctionN for N >= 22
   */
  def isXXLFunctionClass(cls: Symbol) = scalaClassName(cls).functionArity > MaxImplementedFunctionArity

  /** Is a synthetic function class
   *    - FunctionN for N > 22
   *    - ImplicitFunctionN for N > 0
   */
  def isSyntheticFunctionClass(cls: Symbol) = scalaClassName(cls).isSyntheticFunction

  def isAbstractFunctionClass(cls: Symbol) = isVarArityClass(cls, str.AbstractFunction)
  def isTupleClass(cls: Symbol) = isVarArityClass(cls, str.Tuple)
  def isProductClass(cls: Symbol) = isVarArityClass(cls, str.Product)

  /** Returns the erased class of the function class `cls`
   *    - FunctionN for N > 22 becomes FunctionXXL
   *    - FunctionN for 22 > N >= 0 remains as FunctionN
   *    - ImplicitFunctionN for N > 22 becomes FunctionXXL
   *    - ImplicitFunctionN for 22 > N >= 0 becomes FunctionN
   *    - anything else becomes a NoSymbol
   */
  def erasedFunctionClass(cls: Symbol): Symbol = {
    val arity = scalaClassName(cls).functionArity
    if (arity > 22) FunctionXXLClass
    else if (arity >= 0) FunctionClass(arity)
    else NoSymbol
  }

  /** Returns the erased type of the function class `cls`
   *    - FunctionN for N > 22 becomes FunctionXXL
   *    - FunctionN for 22 > N >= 0 remains as FunctionN
   *    - ImplicitFunctionN for N > 22 becomes FunctionXXL
   *    - ImplicitFunctionN for 22 > N >= 0 becomes FunctionN
   *    - anything else becomes a NoType
   */
  def erasedFunctionType(cls: Symbol): Type = {
    val arity = scalaClassName(cls).functionArity
    if (arity > 22) defn.FunctionXXLType
    else if (arity >= 0) defn.FunctionType(arity)
    else NoType
  }

  val predefClassNames: Set[Name] =
    Set("Predef$", "DeprecatedPredef", "LowPriorityImplicits").map(_.toTypeName.unmangleClassName)

  /** Is `cls` the predef module class, or a class inherited by Predef? */
  def isPredefClass(cls: Symbol) =
    (cls.owner eq ScalaPackageClass) && predefClassNames.contains(cls.name)

  val StaticRootImportFns = List[() => TermRef](
    () => JavaLangPackageVal.termRef,
    () => ScalaPackageVal.termRef
  )

  val PredefImportFns = List[() => TermRef](
    () => ScalaPredefModuleRef,
    () => DottyPredefModuleRef
  )

  lazy val RootImportFns =
    if (ctx.settings.YnoImports.value) List.empty[() => TermRef]
    else if (ctx.settings.YnoPredef.value) StaticRootImportFns
    else StaticRootImportFns ++ PredefImportFns

  lazy val ShadowableImportNames = Set("Predef", "DottyPredef").map(_.toTermName)
  lazy val RootImportTypes = RootImportFns.map(_())

  /** Modules whose members are in the default namespace and their module classes */
  lazy val UnqualifiedOwnerTypes: Set[NamedType] =
    RootImportTypes.toSet[NamedType] ++ RootImportTypes.map(_.symbol.moduleClass.typeRef)

  lazy val NotRuntimeClasses = Set[Symbol](AnyClass, AnyValClass, NullClass, NothingClass)

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
  lazy val NoInitClasses = NotRuntimeClasses + FunctionXXLClass

  def isPolymorphicAfterErasure(sym: Symbol) =
     (sym eq Any_isInstanceOf) || (sym eq Any_asInstanceOf)

  def isTupleType(tp: Type)(implicit ctx: Context) = {
    val arity = tp.dealias.argInfos.length
    arity <= MaxTupleArity && TupleType(arity) != null && (tp isRef TupleType(arity).symbol)
  }

  def tupleType(elems: List[Type]) = {
    TupleType(elems.size).appliedTo(elems)
  }

  def isProductSubType(tp: Type)(implicit ctx: Context) =
    tp.derivesFrom(ProductType.symbol)

  /** Is `tp` (an alias) of either a scala.FunctionN or a scala.ImplicitFunctionN? */
  def isFunctionType(tp: Type)(implicit ctx: Context) = {
    val arity = functionArity(tp)
    val sym = tp.dealias.typeSymbol
    arity >= 0 && isFunctionClass(sym) && tp.isRef(FunctionType(arity, sym.name.isImplicitFunction).typeSymbol)
  }

  def functionArity(tp: Type)(implicit ctx: Context) = tp.dealias.argInfos.length - 1

  def isImplicitFunctionType(tp: Type)(implicit ctx: Context) =
    isFunctionType(tp) && tp.dealias.typeSymbol.name.isImplicitFunction

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

  lazy val ScalaNumericValueTypeList = List(
    ByteType, ShortType, CharType, IntType, LongType, FloatType, DoubleType)

  private lazy val ScalaNumericValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypeList.toSet
  private lazy val ScalaValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypes + UnitType + BooleanType
  private lazy val ScalaBoxedTypes = ScalaValueTypes map (t => boxedTypes(t.name))

  val ScalaNumericValueClasses = new PerRun[collection.Set[Symbol]](implicit ctx => ScalaNumericValueTypes.map(_.symbol))
  val ScalaValueClasses        = new PerRun[collection.Set[Symbol]](implicit ctx => ScalaValueTypes.map(_.symbol))
  val ScalaBoxedClasses        = new PerRun[collection.Set[Symbol]](implicit ctx => ScalaBoxedTypes.map(_.symbol))

  private val boxedTypes = mutable.Map[TypeName, TypeRef]()
  private val valueTypeEnc = mutable.Map[TypeName, PrimitiveClassEnc]()

//  private val unboxedTypeRef = mutable.Map[TypeName, TypeRef]()
//  private val javaTypeToValueTypeRef = mutable.Map[Class[_], TypeRef]()
//  private val valueTypeNameToJavaType = mutable.Map[TypeName, Class[_]]()

  private def valueTypeRef(name: String, boxed: TypeRef, jtype: Class[_], enc: Int): TypeRef = {
    val vcls = ctx.requiredClassRef(name)
    boxedTypes(vcls.name) = boxed
    valueTypeEnc(vcls.name) = enc
//    unboxedTypeRef(boxed.name) = vcls
//    javaTypeToValueTypeRef(jtype) = vcls
//    valueTypeNameToJavaType(vcls.name) = jtype
    vcls
  }

  /** The type of the boxed class corresponding to primitive value type `tp`. */
  def boxedType(tp: Type)(implicit ctx: Context): TypeRef = boxedTypes(scalaClassName(tp))

  type PrimitiveClassEnc = Int

  val ByteEnc = 2
  val ShortEnc = ByteEnc * 3
  val CharEnc = 5
  val IntEnc = ShortEnc * CharEnc
  val LongEnc = IntEnc * 7
  val FloatEnc = LongEnc * 11
  val DoubleEnc = FloatEnc * 13
  val BooleanEnc = 17
  val UnitEnc = 19

  def isValueSubType(tref1: TypeRef, tref2: TypeRef)(implicit ctx: Context) =
    valueTypeEnc(tref2.name) % valueTypeEnc(tref1.name) == 0
  def isValueSubClass(sym1: Symbol, sym2: Symbol) =
    valueTypeEnc(sym2.asClass.name) % valueTypeEnc(sym1.asClass.name) == 0

  // ----- Initialization ---------------------------------------------------

  /** Lists core classes that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  lazy val syntheticScalaClasses = List(
    AnyClass,
    AnyRefAlias,
    RepeatedParamClass,
    ByNameParamClass2x,
    AnyValClass,
    NullClass,
    NothingClass,
    SingletonClass,
    EqualsPatternClass,
    PhantomClass)

  lazy val syntheticCoreClasses = syntheticScalaClasses ++ List(
    EmptyPackageVal,
    OpsPackageClass)

  /** Lists core methods that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  lazy val syntheticCoreMethods = AnyMethods ++ ObjectMethods ++ List(String_+, throwMethod)

  lazy val reservedScalaClassNames: Set[Name] = syntheticScalaClasses.map(_.name).toSet

  private[this] var _isInitialized = false
  private def isInitialized = _isInitialized

  def init()(implicit ctx: Context) = {
    this.ctx = ctx
    if (!_isInitialized) {
      // force initialization of every symbol that is synthesized or hijacked by the compiler
      val forced = syntheticCoreClasses ++ syntheticCoreMethods ++ ScalaValueClasses()

      // Enter all symbols from the scalaShadowing package in the scala package
      for (m <- ScalaShadowingPackageClass.info.decls)
        ScalaPackageClass.enter(m)

      _isInitialized = true
    }
  }

  // ----- Phantoms ---------------------------------------------------------

  lazy val PhantomClass: ClassSymbol = {
    val cls = completeClass(enterCompleteClassSymbol(ScalaPackageClass, tpnme.Phantom, NoInitsTrait, List(AnyType)))

    val any = enterCompleteClassSymbol(cls, tpnme.Any, Protected | Final | NoInitsTrait, Nil)
    val nothing = enterCompleteClassSymbol(cls, tpnme.Nothing, Protected | Final | NoInitsTrait, List(any.typeRef))
    enterMethod(cls, nme.assume_, ExprType(nothing.typeRef), Protected | Final | Method)

    cls
  }
  lazy val Phantom_AnyClass = PhantomClass.unforcedDecls.find(_.name eq tpnme.Any).asClass
  lazy val Phantom_NothingClass = PhantomClass.unforcedDecls.find(_.name eq tpnme.Nothing).asClass
  lazy val Phantom_assume = PhantomClass.unforcedDecls.find(_.name eq nme.assume_)

  /** If the symbol is of the class scala.Phantom.Any or scala.Phantom.Nothing */
  def isPhantomTerminalClass(sym: Symbol) = (sym eq Phantom_AnyClass) || (sym eq Phantom_NothingClass)


  lazy val ErasedPhantomType: TypeRef = ctx.requiredClassRef("dotty.runtime.ErasedPhantom")
  def ErasedPhantomClass(implicit ctx: Context) = ErasedPhantomType.symbol.asClass

  def ErasedPhantom_UNIT(implicit ctx: Context) = ErasedPhantomClass.linkedClass.requiredValue("UNIT")

}
