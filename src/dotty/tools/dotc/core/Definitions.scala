package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Denotations._, SymDenotations._, StdNames._, Names._
import Flags._, Scopes._, Decorators._, NameOps._, util.Positions._
import unpickleScala2.Scala2Unpickler.ensureConstructor
import scala.annotation.{ switch, meta }
import scala.collection.{ mutable, immutable }
import PartialFunction._
import collection.mutable
import scala.reflect.api.{ Universe => ApiUniverse }

object Definitions {
  val MaxFunctionArity, MaxTupleArity = 22
}

/** A class defining symbols and types of standard definitions */
class Definitions {
  import Definitions._

  private implicit var ctx: Context = _

  private def newSymbol[N <: Name](owner: Symbol, name: N, flags: FlagSet, info: Type) =
    ctx.newSymbol(owner, name, flags | Permanent, info)

  private def newClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet, infoFn: ClassSymbol => Type) =
    ctx.newClassSymbol(owner, name, flags | Permanent, infoFn).entered

  private def newCompleteClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet, parents: List[TypeRef], decls: Scope = newScope) =
    ctx.newCompleteClassSymbol(owner, name, flags | Permanent, parents, decls).entered

  private def newTopClassSymbol(name: TypeName, flags: FlagSet, parents: List[TypeRef]) =
    completeClass(newCompleteClassSymbol(ScalaPackageClass, name, flags, parents))

  private def newTypeField(cls: ClassSymbol, name: TypeName, flags: FlagSet, scope: MutableScope) =
    scope.enter(newSymbol(cls, name, flags, TypeBounds.empty))

  private def newTypeParam(cls: ClassSymbol, name: TypeName, flags: FlagSet, scope: MutableScope) =
    newTypeField(cls, name, flags | ClassTypeParamCreationFlags, scope)

  private def newSyntheticTypeParam(cls: ClassSymbol, scope: MutableScope, paramFlags: FlagSet, suffix: String = "T0") =
    newTypeParam(cls, suffix.toTypeName.expandedName(cls), ExpandedName | paramFlags, scope)

  private def specialPolyClass(name: TypeName, paramFlags: FlagSet, parentConstrs: Type*): ClassSymbol = {
    val completer = new LazyType {
      def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
        val cls = denot.asClass.classSymbol
        val paramDecls = newScope
        val typeParam = newSyntheticTypeParam(cls, paramDecls, paramFlags)
        def instantiate(tpe: Type) =
          if (tpe.typeParams.nonEmpty) tpe.appliedTo(typeParam.typeRef)
          else tpe
        val parents = parentConstrs.toList map instantiate
        val parentRefs: List[TypeRef] = ctx.normalizeToClassRefs(parents, cls, paramDecls)
        denot.info = ClassInfo(ScalaPackageClass.thisType, cls, parentRefs, paramDecls)
      }
    }
    newClassSymbol(ScalaPackageClass, name, EmptyFlags, completer)
  }

  private def newMethod(cls: ClassSymbol, name: TermName, info: Type, flags: FlagSet = EmptyFlags): TermSymbol =
    newSymbol(cls, name.encode, flags | Method, info).entered.asTerm

  private def newAliasType(name: TypeName, tpe: Type, flags: FlagSet = EmptyFlags): TypeSymbol = {
    val sym = newSymbol(ScalaPackageClass, name, flags, TypeAlias(tpe))
    ScalaPackageClass.currentPackageDecls.enter(sym)
    sym
  }

  private def newPolyMethod(cls: ClassSymbol, name: TermName, typeParamCount: Int,
                    resultTypeFn: PolyType => Type, flags: FlagSet = EmptyFlags) = {
    val tparamNames = tpnme.syntheticTypeParamNames(typeParamCount)
    val tparamBounds = tparamNames map (_ => TypeBounds.empty)
    val ptype = PolyType(tparamNames)(_ => tparamBounds, resultTypeFn)
    newMethod(cls, name, ptype, flags)
  }

  private def newT1ParameterlessMethod(cls: ClassSymbol, name: TermName, resultTypeFn: PolyType => Type, flags: FlagSet) =
    newPolyMethod(cls, name, 1, resultTypeFn, flags)

  private def newT1EmptyParamsMethod(cls: ClassSymbol, name: TermName, resultTypeFn: PolyType => Type, flags: FlagSet) =
    newPolyMethod(cls, name, 1, pt => MethodType(Nil, resultTypeFn(pt)), flags)

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
  lazy val ScalaPackageClass = ScalaPackageVal.moduleClass.asClass
  lazy val JavaPackageVal = ctx.requiredPackage("java")
  lazy val JavaLangPackageVal = ctx.requiredPackage("java.lang")
  // fundamental modules
  lazy val SysPackage = ctx.requiredModule("scala.sys.package")
    lazy val Sys_errorR = ctx.requiredMethodRef(SysPackage.moduleClass.typeRef, nme.error)
    def Sys_error = Sys_errorR.symbol

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
  lazy val AnyClass: ClassSymbol = completeClass(newCompleteClassSymbol(ScalaPackageClass, tpnme.Any, Abstract, Nil))
  lazy val AnyValClass: ClassSymbol = completeClass(newCompleteClassSymbol(ScalaPackageClass, tpnme.AnyVal, Abstract, List(AnyClass.typeRef)))

    lazy val Any_==       = newMethod(AnyClass, nme.EQ, methOfAny(BooleanType), Final)
    lazy val Any_!=       = newMethod(AnyClass, nme.NE, methOfAny(BooleanType), Final)
    lazy val Any_equals   = newMethod(AnyClass, nme.equals_, methOfAny(BooleanType))
    lazy val Any_hashCode = newMethod(AnyClass, nme.hashCode_, MethodType(Nil, IntType))
    lazy val Any_toString = newMethod(AnyClass, nme.toString_, MethodType(Nil, StringType))
    lazy val Any_##       = newMethod(AnyClass, nme.HASHHASH, ExprType(IntType), Final)
    lazy val Any_getClass = newMethod(AnyClass, nme.getClass_, MethodType(Nil, ClassClass.typeRef), Final)
    lazy val Any_isInstanceOf = newT1ParameterlessMethod(AnyClass, nme.isInstanceOf_, _ => BooleanType, Final)
    lazy val Any_asInstanceOf = newT1ParameterlessMethod(AnyClass, nme.asInstanceOf_, PolyParam(_, 0), Final)

    def AnyMethods = List(Any_==, Any_!=, Any_equals, Any_hashCode,
      Any_toString, Any_##, Any_getClass, Any_isInstanceOf, Any_asInstanceOf)

  lazy val ObjectClass: ClassSymbol = {
    val cls = ctx.requiredClass("java.lang.Object")
    assert(!cls.isCompleted, "race for completing java.lang.Object")
    cls.info = ClassInfo(cls.owner.thisType, cls, AnyClass.typeRef :: Nil, newScope)
    completeClass(cls)
  }

  lazy val AnyRefAlias: TypeSymbol = newAliasType(tpnme.AnyRef, ObjectType)

    lazy val Object_eq = newMethod(ObjectClass, nme.eq, methOfAnyRef(BooleanType), Final)
    lazy val Object_ne = newMethod(ObjectClass, nme.ne, methOfAnyRef(BooleanType), Final)
    lazy val Object_synchronized = newPolyMethod(ObjectClass, nme.synchronized_, 1,
        pt => MethodType(List(PolyParam(pt, 0)), PolyParam(pt, 0)), Final)
    lazy val Object_clone = newMethod(ObjectClass, nme.clone_, MethodType(Nil, ObjectType), Protected)
    lazy val Object_finalize = newMethod(ObjectClass, nme.finalize_, MethodType(Nil, UnitType), Protected)
    lazy val Object_notify = newMethod(ObjectClass, nme.notify_, MethodType(Nil, UnitType))
    lazy val Object_notifyAll = newMethod(ObjectClass, nme.notifyAll_, MethodType(Nil, UnitType))
    lazy val Object_wait = newMethod(ObjectClass, nme.wait_, MethodType(Nil, UnitType))
    lazy val Object_waitL = newMethod(ObjectClass, nme.wait_, MethodType(LongType :: Nil, UnitType))
    lazy val Object_waitLI = newMethod(ObjectClass, nme.wait_, MethodType(LongType :: IntType :: Nil, UnitType))

    def ObjectMethods = List(Object_eq, Object_ne, Object_synchronized, Object_clone,
        Object_finalize, Object_notify, Object_notifyAll, Object_wait, Object_waitL, Object_waitLI)

  /** Dummy method needed by elimByName */
  lazy val dummyApply = newPolyMethod(
      OpsPackageClass, nme.dummyApply, 1,
      pt => MethodType(List(FunctionType(Nil, PolyParam(pt, 0))), PolyParam(pt, 0)))

  /** Method representing a throw */
  lazy val throwMethod = newMethod(OpsPackageClass, nme.THROWkw,
      MethodType(List(ThrowableType), NothingType))

  lazy val NothingClass: ClassSymbol = newCompleteClassSymbol(
    ScalaPackageClass, tpnme.Nothing, AbstractFinal, List(AnyClass.typeRef))
  lazy val NullClass: ClassSymbol = newCompleteClassSymbol(
    ScalaPackageClass, tpnme.Null, AbstractFinal, List(ObjectClass.typeRef))
  lazy val NullTypeRef = NullClass.typeRef

  lazy val ScalaPredefModuleRef = ctx.requiredModuleRef("scala.Predef")
  def ScalaPredefModule = ScalaPredefModuleRef.symbol

    lazy val Predef_conformsR = ctx.requiredMethodRef(ScalaPredefModuleRef, "$conforms")
    def Predef_conforms = Predef_conformsR.symbol

  lazy val ScalaRuntimeModuleRef = ctx.requiredModuleRef("scala.runtime.ScalaRunTime")
  def ScalaRuntimeModule = ScalaRuntimeModuleRef.symbol
  def ScalaRuntimeClass = ScalaRuntimeModule.moduleClass.asClass

    def runtimeMethodRef(name: PreName) = ctx.requiredMethodRef(ScalaRuntimeModuleRef, name)
    def ScalaRuntime_dropR = runtimeMethodRef(nme.drop)
    def ScalaRuntime_drop = ScalaRuntime_dropR.symbol

  lazy val BoxesRunTimeModuleRef = ctx.requiredModuleRef("scala.runtime.BoxesRunTime")
  def BoxesRunTimeModule = BoxesRunTimeModuleRef.symbol
  def BoxesRunTimeClass = BoxesRunTimeModule.moduleClass.asClass
  lazy val ScalaStaticsModuleRef = ctx.requiredModuleRef("scala.runtime.Statics")
  def ScalaStaticsModule = ScalaStaticsModuleRef.symbol
  def ScalaStaticsClass = ScalaStaticsModule.moduleClass.asClass

    def staticsMethodRef(name: PreName) = ctx.requiredMethodRef(ScalaStaticsModuleRef, name)
    def staticsMethod(name: PreName) = ctx.requiredMethod(ScalaStaticsModuleRef, name)

  lazy val DottyPredefModuleRef = ctx.requiredModuleRef("dotty.DottyPredef")
  def DottyPredefModule = DottyPredefModuleRef.symbol
  lazy val DottyArraysModuleRef = ctx.requiredModuleRef("dotty.runtime.Arrays")
  def DottyArraysModule = DottyArraysModuleRef.symbol

    def newRefArrayMethod = ctx.requiredMethod(DottyArraysModuleRef, "newRefArray")

  lazy val NilModuleRef = ctx.requiredModuleRef("scala.collection.immutable.Nil")
  def NilModule = NilModuleRef.symbol

//  lazy val FunctionClass: ClassSymbol = ctx.requiredClass("scala.Function")
  lazy val SingletonClass: ClassSymbol =
    // needed as a synthetic class because Scala 2.x refers to it in classfiles
    // but does not define it as an explicit class.
    newCompleteClassSymbol(
      ScalaPackageClass, tpnme.Singleton, PureInterfaceCreationFlags | Final,
      List(AnyClass.typeRef), EmptyScope)

  lazy val SeqTypeRef = ctx.requiredClassRef("scala.collection.Seq")
  def SeqClass = SeqTypeRef.symbol.asClass
    lazy val Seq_applyR = ctx.requiredMethodRef(SeqTypeRef, nme.apply)
    def Seq_apply = Seq_applyR.symbol
    lazy val Seq_headR = ctx.requiredMethodRef(SeqTypeRef, nme.head)
    def Seq_head = Seq_headR.symbol

  lazy val ArrayTypeRef = ctx.requiredClassRef("scala.Array")
  def ArrayClass = ArrayTypeRef.symbol.asClass
    lazy val Array_applyR                 = ctx.requiredMethodRef(ArrayTypeRef, nme.apply)
    def Array_apply = Array_applyR.symbol
    lazy val Array_updateR                = ctx.requiredMethodRef(ArrayTypeRef, nme.update)
    def Array_update = Array_updateR.symbol
    lazy val Array_lengthR                = ctx.requiredMethodRef(ArrayTypeRef, nme.length)
    def Array_length = Array_lengthR.symbol
    lazy val Array_cloneR                 = ctx.requiredMethodRef(ArrayTypeRef, nme.clone_)
    def Array_clone = Array_cloneR.symbol
    lazy val ArrayConstructorR            = ctx.requiredMethodRef(ArrayTypeRef, nme.CONSTRUCTOR)
    def ArrayConstructor = ArrayConstructorR.symbol

  lazy val UnitTypeRef = valueTypeRef("scala.Unit", BoxedUnitTypeRef, java.lang.Void.TYPE, UnitEnc)
  def UnitClass = UnitTypeRef.symbol.asClass
  lazy val BooleanTypeRef = valueTypeRef("scala.Boolean", BoxedBooleanTypeRef, java.lang.Boolean.TYPE, BooleanEnc)
  def BooleanClass = BooleanTypeRef.symbol.asClass
    lazy val Boolean_notR   = BooleanTypeRef.symbol.requiredMethodRef(nme.UNARY_!)
    def Boolean_! = Boolean_notR.symbol
    lazy val Boolean_andR = BooleanTypeRef.symbol.requiredMethodRef(nme.ZAND) // ### harmonize required... calls
    def Boolean_&& = Boolean_andR.symbol
    lazy val Boolean_orR  = BooleanTypeRef.symbol.requiredMethodRef(nme.ZOR)
    def Boolean_|| = Boolean_orR.symbol

  lazy val ByteTypeRef = valueTypeRef("scala.Byte", BoxedByteTypeRef, java.lang.Byte.TYPE, ByteEnc)
  def ByteClass = ByteTypeRef.symbol.asClass
  lazy val ShortTypeRef = valueTypeRef("scala.Short", BoxedShortTypeRef, java.lang.Short.TYPE, ShortEnc)
  def ShortClass = ShortTypeRef.symbol.asClass
  lazy val CharTypeRef = valueTypeRef("scala.Char", BoxedCharTypeRef, java.lang.Character.TYPE, CharEnc)
  def CharClass = CharTypeRef.symbol.asClass
  lazy val IntTypeRef = valueTypeRef("scala.Int", BoxedIntTypeRef, java.lang.Integer.TYPE, IntEnc)
  def IntClass = IntTypeRef.symbol.asClass
    lazy val Int_minusR   = IntTypeRef.symbol.requiredMethodRef(nme.MINUS, List(IntType))
    def Int_- = Int_minusR.symbol
    lazy val Int_plusR   = IntTypeRef.symbol.requiredMethodRef(nme.PLUS, List(IntType))
    def Int_+ = Int_plusR.symbol
    lazy val Int_divR   = IntTypeRef.symbol.requiredMethodRef(nme.DIV, List(IntType))
    def Int_/ = Int_divR.symbol
    lazy val Int_mulR   = IntTypeRef.symbol.requiredMethodRef(nme.MUL, List(IntType))
    def Int_* = Int_mulR.symbol
    lazy val Int_eqR   = IntTypeRef.symbol.requiredMethodRef(nme.EQ, List(IntType))
    def Int_== = Int_eqR.symbol
    lazy val Int_geR   = IntTypeRef.symbol.requiredMethodRef(nme.GE, List(IntType))
    def Int_>= = Int_geR.symbol
    lazy val Int_leR   = IntTypeRef.symbol.requiredMethodRef(nme.LE, List(IntType))
    def Int_<= = Int_leR.symbol
  lazy val LongTypeRef = valueTypeRef("scala.Long", BoxedLongTypeRef, java.lang.Long.TYPE, LongEnc)
  def LongClass = LongTypeRef.symbol.asClass
    lazy val Long_XOR_Long = LongTypeRef.member(nme.XOR).requiredSymbol(
      x => (x is Method) && (x.info.firstParamTypes.head isRef defn.LongClass)
    )
    lazy val Long_LSR_Int = LongTypeRef.member(nme.LSR).requiredSymbol(
      x => (x is Method) && (x.info.firstParamTypes.head isRef defn.IntClass)
    )
  lazy val FloatTypeRef = valueTypeRef("scala.Float", BoxedFloatTypeRef, java.lang.Float.TYPE, FloatEnc)
  def FloatClass = FloatTypeRef.symbol.asClass
  lazy val DoubleTypeRef = valueTypeRef("scala.Double", BoxedDoubleTypeRef, java.lang.Double.TYPE, DoubleEnc)
  def DoubleClass = DoubleTypeRef.symbol.asClass

  lazy val BoxedUnitTypeRef = ctx.requiredClassRef("scala.runtime.BoxedUnit")
  def BoxedUnitClass = BoxedUnitTypeRef.symbol.asClass

    lazy val BoxedUnit_UNIT = BoxedUnitClass.linkedClass.requiredValue("UNIT")

  lazy val BoxedBooleanTypeRef = ctx.requiredClassRef("java.lang.Boolean")
  def BoxedBooleanClass = BoxedBooleanTypeRef.symbol.asClass
  lazy val BoxedByteTypeRef = ctx.requiredClassRef("java.lang.Byte")
  def BoxedByteClass = BoxedByteTypeRef.symbol.asClass
  lazy val BoxedShortTypeRef = ctx.requiredClassRef("java.lang.Short")
  def BoxedShortClass = BoxedShortTypeRef.symbol.asClass
  lazy val BoxedCharTypeRef = ctx.requiredClassRef("java.lang.Character")
  def BoxedCharClass = BoxedCharTypeRef.symbol.asClass
  lazy val BoxedIntTypeRef = ctx.requiredClassRef("java.lang.Integer")
  def BoxedIntClass = BoxedIntTypeRef.symbol.asClass
  lazy val BoxedLongTypeRef = ctx.requiredClassRef("java.lang.Long")
  def BoxedLongClass = BoxedLongTypeRef.symbol.asClass
  lazy val BoxedFloatTypeRef = ctx.requiredClassRef("java.lang.Float")
  def BoxedFloatClass = BoxedFloatTypeRef.symbol.asClass
  lazy val BoxedDoubleTypeRef = ctx.requiredClassRef("java.lang.Double")
  def BoxedDoubleClass = BoxedDoubleTypeRef.symbol.asClass

  lazy val BoxedBooleanModule = ctx.requiredModule("java.lang.Boolean")
  lazy val BoxedByteModule    = ctx.requiredModule("java.lang.Byte")
  lazy val BoxedShortModule   = ctx.requiredModule("java.lang.Short")
  lazy val BoxedCharModule    = ctx.requiredModule("java.lang.Character")
  lazy val BoxedIntModule     = ctx.requiredModule("java.lang.Integer")
  lazy val BoxedLongModule    = ctx.requiredModule("java.lang.Long")
  lazy val BoxedFloatModule   = ctx.requiredModule("java.lang.Float")
  lazy val BoxedDoubleModule  = ctx.requiredModule("java.lang.Double")
  lazy val BoxedUnitModule    = ctx.requiredModule("java.lang.Void")

  lazy val ByNameParamClass2x     = specialPolyClass(tpnme.BYNAME_PARAM_CLASS, Covariant, AnyType)
  lazy val EqualsPatternClass     = specialPolyClass(tpnme.EQUALS_PATTERN, EmptyFlags, AnyType)

  lazy val RepeatedParamClass     = specialPolyClass(tpnme.REPEATED_PARAM_CLASS, Covariant, ObjectType, SeqType)

  // fundamental classes
  lazy val StringClass                = ctx.requiredClass("java.lang.String")
  lazy val StringModule               = StringClass.linkedClass

    lazy val String_+ = newMethod(StringClass, nme.raw.PLUS, methOfAny(StringType), Final)
    lazy val String_valueOf_Object = StringModule.info.member(nme.valueOf).suchThat(_.info.firstParamTypes match {
      case List(pt) => (pt isRef AnyClass) || (pt isRef ObjectClass)
      case _ => false
    }).symbol

  lazy val JavaCloneableClass        = ctx.requiredClass("java.lang.Cloneable")
  lazy val NullPointerExceptionClass = ctx.requiredClass("java.lang.NullPointerException")
  lazy val ClassClass                = ctx.requiredClass("java.lang.Class")
  lazy val BoxedNumberClass          = ctx.requiredClass("java.lang.Number")
  lazy val ThrowableClass            = ctx.requiredClass("java.lang.Throwable")
  lazy val ClassCastExceptionClass   = ctx.requiredClass("java.lang.ClassCastException")
  lazy val JavaSerializableClass     = ctx.requiredClass("java.lang.Serializable")
  lazy val ComparableClass           = ctx.requiredClass("java.lang.Comparable")

  // in scalac modified to have Any as parent

  lazy val SerializableTypeRef       = ctx.requiredClassRef("scala.Serializable")
  def SerializableClass = SerializableTypeRef.symbol.asClass
  lazy val StringBuilderTypeRef      = ctx.requiredClassRef("scala.collection.mutable.StringBuilder")
  def StringBuilderClass = StringBuilderTypeRef.symbol.asClass
  lazy val MatchErrorTypeRef         = ctx.requiredClassRef("scala.MatchError")
  def MatchErrorClass = MatchErrorTypeRef.symbol.asClass

  lazy val StringAddTypeRef          = ctx.requiredClassRef("scala.runtime.StringAdd")
  def StringAddClass = StringAddTypeRef.symbol.asClass

    lazy val StringAdd_plusR = StringAddTypeRef.symbol.requiredMethodRef(nme.raw.PLUS)
    def StringAdd_+ = StringAdd_plusR.symbol

  lazy val PairTypeRef                    = ctx.requiredClassRef("dotty.Pair")
  def PairClass = PairTypeRef.symbol.asClass
  lazy val PartialFunctionTypeRef         = ctx.requiredClassRef("scala.PartialFunction")
  def PartialFunctionClass = PartialFunctionTypeRef.symbol.asClass
  lazy val AbstractPartialFunctionTypeRef = ctx.requiredClassRef("scala.runtime.AbstractPartialFunction")
  def AbstractPartialFunctionClass = AbstractPartialFunctionTypeRef.symbol.asClass
  lazy val SymbolTypeRef                  = ctx.requiredClassRef("scala.Symbol")
  def SymbolClass = SymbolTypeRef.symbol.asClass
  lazy val DynamicTypeRef                 = ctx.requiredClassRef("scala.Dynamic")
  def DynamicClass = DynamicTypeRef.symbol.asClass
  lazy val OptionTypeRef                  = ctx.requiredClassRef("scala.Option")
  def OptionClass = OptionTypeRef.symbol.asClass
  lazy val ProductTypeRef                 = ctx.requiredClassRef("scala.Product")
  def ProductClass = ProductTypeRef.symbol.asClass
    lazy val Product_canEqualR = ProductTypeRef.symbol.requiredMethodRef(nme.canEqual_)
    def Product_canEqual = Product_canEqualR.symbol
    lazy val Product_productArityR = ProductTypeRef.symbol.requiredMethod(nme.productArity)
    def Product_productArity = Product_productArityR.symbol
    lazy val Product_productPrefixR = ProductTypeRef.symbol.requiredMethod(nme.productPrefix)
    def Product_productPrefix = Product_productPrefixR.symbol
  lazy val LanguageModuleTypeRef          = ctx.requiredModule("dotty.language").moduleClass.asClass
  def LanguageModuleClass = LanguageModuleTypeRef.symbol.asClass // ### Needed? Maube just keep LanguageModule?
  lazy val NonLocalReturnControlTypeRef   = ctx.requiredClassRef("scala.runtime.NonLocalReturnControl")

  // Annotation base classes
  lazy val AnnotationTypeRef              = ctx.requiredClassRef("scala.annotation.Annotation") // ### AnnotationTypeRef --> AnnotRef?
  def AnnotationClass = AnnotationTypeRef.symbol.asClass
  lazy val ClassfileAnnotationTypeRef     = ctx.requiredClassRef("scala.annotation.ClassfileAnnotation")
  def ClassfileAnnotationClass = ClassfileAnnotationTypeRef.symbol.asClass
  lazy val StaticAnnotationTypeRef        = ctx.requiredClassRef("scala.annotation.StaticAnnotation")
  def StaticAnnotationClass = StaticAnnotationTypeRef.symbol.asClass
  lazy val TailrecAnnotationTypeRef       = ctx.requiredClassRef("scala.annotation.tailrec")
  def TailrecAnnotationClass = TailrecAnnotationTypeRef.symbol.asClass
  lazy val RemoteAnnotRef                   = ctx.requiredClassRef("scala.remote")
  def RemoteAnnot = RemoteAnnotRef.symbol.asClass
  lazy val SerialVersionUIDAnnotRef         = ctx.requiredClassRef("scala.SerialVersionUID")
  def SerialVersionUIDAnnot = SerialVersionUIDAnnotRef.symbol.asClass
  lazy val TransientAnnotRef                = ctx.requiredClassRef("scala.transient")
  def TransientAnnot = TransientAnnotRef.symbol.asClass
  lazy val NativeAnnotRef                   = ctx.requiredClassRef("scala.native")
  def NativeAnnot = NativeAnnotRef.symbol.asClass
  lazy val ScalaStrictFPAnnotRef            = ctx.requiredClassRef("scala.annotation.strictfp")
  def ScalaStrictFPAnnot = ScalaStrictFPAnnotRef.symbol.asClass

  // Annotation classes
  lazy val AliasAnnotRef = ctx.requiredClassRef("dotty.annotation.internal.Alias")
  def AliasAnnot = AliasAnnotRef.symbol.asClass
  lazy val ChildAnnotRef = ctx.requiredClassRef("dotty.annotation.internal.Child")
  def ChildAnnot = ChildAnnotRef.symbol.asClass
  lazy val RepeatedAnnotRef = ctx.requiredClassRef("dotty.annotation.internal.Repeated")
  def RepeatedAnnot = RepeatedAnnotRef.symbol.asClass
  lazy val InvariantBetweenAnnotRef = ctx.requiredClassRef("dotty.annotation.internal.InvariantBetween")
  def InvariantBetweenAnnot = InvariantBetweenAnnotRef.symbol.asClass
  lazy val CovariantBetweenAnnotRef = ctx.requiredClassRef("dotty.annotation.internal.CovariantBetween")
  def CovariantBetweenAnnot = CovariantBetweenAnnotRef.symbol.asClass
  lazy val ContravariantBetweenAnnotRef = ctx.requiredClassRef("dotty.annotation.internal.ContravariantBetween")
  def ContravariantBetweenAnnot = ContravariantBetweenAnnotRef.symbol.asClass
  lazy val ScalaSignatureAnnotRef = ctx.requiredClassRef("scala.reflect.ScalaSignature")
  def ScalaSignatureAnnot = ScalaSignatureAnnotRef.symbol.asClass
  lazy val ScalaLongSignatureAnnotRef = ctx.requiredClassRef("scala.reflect.ScalaLongSignature")
  def ScalaLongSignatureAnnot = ScalaLongSignatureAnnotRef.symbol.asClass
  lazy val TASTYSignatureAnnotRef = ctx.requiredClassRef("scala.annotation.internal.TASTYSignature")
  def TASTYSignatureAnnot = TASTYSignatureAnnotRef.symbol.asClass
  lazy val TASTYLongSignatureAnnotRef = ctx.requiredClassRef("scala.annotation.internal.TASTYLongSignature")
  def TASTYLongSignatureAnnot = TASTYLongSignatureAnnotRef.symbol.asClass
  lazy val DeprecatedAnnotRef = ctx.requiredClassRef("scala.deprecated")
  def DeprecatedAnnot = DeprecatedAnnotRef.symbol.asClass
  lazy val MigrationAnnotRef = ctx.requiredClassRef("scala.annotation.migration")
  def MigrationAnnot = MigrationAnnotRef.symbol.asClass
  lazy val AnnotationDefaultAnnotRef = ctx.requiredClassRef("dotty.annotation.internal.AnnotationDefault")
  def AnnotationDefaultAnnot = AnnotationDefaultAnnotRef.symbol.asClass
  lazy val ThrowsAnnotRef = ctx.requiredClassRef("scala.throws")
  def ThrowsAnnot = ThrowsAnnotRef.symbol.asClass
  lazy val UncheckedAnnotRef = ctx.requiredClassRef("scala.unchecked")
  def UncheckedAnnot = UncheckedAnnotRef.symbol.asClass
  lazy val UncheckedStableAnnotRef = ctx.requiredClassRef("scala.annotation.unchecked.uncheckedStable")
  def UncheckedStableAnnot = UncheckedStableAnnotRef.symbol.asClass
  lazy val UncheckedVarianceAnnotRef = ctx.requiredClassRef("scala.annotation.unchecked.uncheckedVariance")
  def UncheckedVarianceAnnot = UncheckedVarianceAnnotRef.symbol.asClass
  lazy val VolatileAnnotRef = ctx.requiredClassRef("scala.volatile")
  def VolatileAnnot = VolatileAnnotRef.symbol.asClass
  lazy val FieldMetaAnnotRef = ctx.requiredClassRef("scala.annotation.meta.field")
  def FieldMetaAnnot = FieldMetaAnnotRef.symbol.asClass
  lazy val GetterMetaAnnotRef = ctx.requiredClassRef("scala.annotation.meta.getter")
  def GetterMetaAnnot = GetterMetaAnnotRef.symbol.asClass
  lazy val SetterMetaAnnotRef = ctx.requiredClassRef("scala.annotation.meta.setter")
  def SetterMetaAnnot = SetterMetaAnnotRef.symbol.asClass

  // convenient one-parameter method types
  def methOfAny(tp: Type) = MethodType(List(AnyType), tp)
  def methOfAnyVal(tp: Type) = MethodType(List(AnyValType), tp)
  def methOfAnyRef(tp: Type) = MethodType(List(ObjectType), tp)

  // Derived types
  def AnyType: Type = AnyClass.typeRef
  def AnyValType: Type = AnyValClass.typeRef
  def ObjectType: Type = ObjectClass.typeRef
  def AnyRefType: Type = AnyRefAlias.typeRef
  def NothingType: Type = NothingClass.typeRef
  def NullType: Type = NullClass.typeRef
  def SeqType: Type = SeqTypeRef

  def UnitType: Type = UnitTypeRef
  def BooleanType: Type = BooleanTypeRef
  def ByteType: Type = ByteTypeRef
  def ShortType: Type = ShortTypeRef
  def CharType: Type = CharTypeRef
  def IntType: Type = IntTypeRef
  def LongType: Type = LongTypeRef
  def FloatType: Type = FloatTypeRef
  def DoubleType: Type = DoubleTypeRef
  def PairType: Type = PairTypeRef
  def StringType: Type = StringClass.typeRef
  def RepeatedParamType = RepeatedParamClass.typeRef
  def ThrowableType = ThrowableClass.typeRef
  def OptionType = OptionTypeRef
  def VolatileAnnotType = VolatileAnnotRef

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

  object FunctionType {
    def apply(args: List[Type], resultType: Type)(implicit ctx: Context) =
      FunctionTypeRef(args.length).appliedTo(args ::: resultType :: Nil)
    def unapply(ft: Type)(implicit ctx: Context)/*: Option[(List[Type], Type)]*/ = {
      // -language:keepUnions difference: unapply needs result type because inferred type
      // is Some[(List[Type], Type)] | None, which is not a legal unapply type.
      val tsym = ft.typeSymbol
      lazy val targs = ft.argInfos
      val numArgs = targs.length - 1
      if (numArgs >= 0 && numArgs <= MaxFunctionArity &&
          (FunctionTypeRef(numArgs).symbol == tsym)) Some(targs.init, targs.last)
      else None
    }
  }

  object ArrayType {
    def apply(elem: Type)(implicit ctx: Context) =
      if (ctx.erasedTypes) JavaArrayType(elem)
      else ArrayTypeRef.appliedTo(elem :: Nil)
    def unapply(tp: Type)(implicit ctx: Context): Option[Type] = tp.dealias match {
      case at: RefinedType if (at isRef ArrayTypeRef.symbol) && at.argInfos.length == 1 => Some(at.argInfos.head)
      case _ => None
    }
  }

  object MultiArrayType {
    def apply(elem: Type, ndims: Int)(implicit ctx: Context): Type =
      if (ndims == 0) elem else ArrayType(apply(elem, ndims - 1))
    def unapply(tp: Type)(implicit ctx: Context): Option[(Type, Int)] = tp match {
      case ArrayType(elemtp) =>
        elemtp match {
          case MultiArrayType(finalElemTp, n) => Some(finalElemTp, n + 1)
          case _ => Some(elemtp, 1)
        }
      case _ =>
        None
    }
  }

  // ----- Symbol sets ---------------------------------------------------

  lazy val AbstractFunctionTypeRef = mkArityArray("scala.runtime.AbstractFunction", MaxFunctionArity, 0)
  def AbstractFunctionClass = AbstractFunctionTypeRef.map(_.symbol.asClass)
  lazy val FunctionTypeRef = mkArityArray("scala.Function", MaxFunctionArity, 0)
  def FunctionClass = FunctionTypeRef.map(_.symbol.asClass)
    lazy val Function0_applyR = FunctionTypeRef(0).symbol.requiredMethodRef(nme.apply)
    def Function0_apply = Function0_applyR.symbol

  lazy val TupleTypeRef = mkArityArray("scala.Tuple", MaxTupleArity, 2)
  lazy val ProductNTypeRef = mkArityArray("scala.Product", MaxTupleArity, 0)

  lazy val FunctionTypeRefs: Set[TypeRef] = FunctionTypeRef.toSet
  lazy val TupleTypeRefs: Set[TypeRef] = TupleTypeRef.toSet
  lazy val ProductTypeRefs: Set[TypeRef] = ProductNTypeRef.toSet

  /** If type refers to a class in the scala package, its name, otherwise EmptyTypeName */
  def scalaClassName(ref: Type)(implicit ctx: Context): TypeName = {
    val cls = ref.classSymbol
    if (cls.isClass && cls.owner == ScalaPackageClass) cls.asClass.name else EmptyTypeName
  }

  def isVarArityClass(cls: Symbol, prefix: Name) = // ### use scalaClassName
    cls.owner == ScalaPackageClass && cls.name.startsWith(prefix) &&
    cls.name.drop(prefix.length).forall(_.isDigit)

  def isFunctionClass(cls: Symbol) = isVarArityClass(cls, tpnme.Function)
  def isAbstractFunctionClass(cls: Symbol) = isVarArityClass(cls, tpnme.AbstractFunction)
  def isTupleClass(cls: Symbol) = isVarArityClass(cls, tpnme.Tuple)
  def isProductClass(cls: Symbol) = isVarArityClass(cls, tpnme.Product)

  val RootImportFns = List[() => TermRef](
      () => JavaLangPackageVal.termRef,
      () => ScalaPackageVal.termRef,
      () => ScalaPredefModuleRef,
      () => DottyPredefModuleRef)

  lazy val RootImportTypes = RootImportFns.map(_())

  /** `Modules whose members are in the default namespace and their module classes */
  lazy val UnqualifiedOwnerTypes: Set[NamedType] =
    RootImportTypes.toSet[NamedType] ++ RootImportTypes.map(_.symbol.moduleClass.typeRef)

  lazy val PhantomClasses = Set[Symbol](AnyClass, AnyValClass, NullClass, NothingClass)

  lazy val isPolymorphicAfterErasure = // ### Can't be a lazy val, because of newRefArrayMethod!
    Set[Symbol](Any_isInstanceOf, Any_asInstanceOf, newRefArrayMethod)

  def isTupleType(tp: Type)(implicit ctx: Context) = {
    val arity = tp.dealias.argInfos.length
    arity <= MaxTupleArity && TupleTypeRef(arity) != null && (tp isRef TupleTypeRef(arity).symbol)
  }

  def tupleType(elems: List[Type]) = {
    TupleTypeRef(elems.size).appliedTo(elems)
  }

  def isProductSubType(tp: Type)(implicit ctx: Context) =
    (tp derivesFrom ProductTypeRef.symbol) && tp.baseClasses.exists(isProductClass)

  def isFunctionType(tp: Type)(implicit ctx: Context) = {
    val arity = functionArity(tp)
    0 <= arity && arity <= MaxFunctionArity && (tp isRef FunctionTypeRef(arity).symbol)
  }

  def functionArity(tp: Type)(implicit ctx: Context) = tp.dealias.argInfos.length - 1

  // ----- LambdaXYZ traits ------------------------------------------

  private var myLambdaTraits: Set[Symbol] = Set()

  /** The set of HigherKindedXYZ traits encountered so far */
  def lambdaTraits: Set[Symbol] = myLambdaTraits

  private var LambdaTraitForVariances = mutable.Map[List[Int], ClassSymbol]()

  /** The HigherKinded trait corresponding to symbols `boundSyms` (which are assumed
   *  to be the type parameters of a higher-kided type). This is a class symbol that
   *  would be generated by the following schema.
   *
   *      class LambdaXYZ extends Object with P1 with ... with Pn {
   *        type v_1 $hk$Arg0; ...; type v_N $hk$ArgN;
   *        type Apply
   *      }
   *
   *  Here:
   *
   *   - v_i are the variances of the bound symbols (i.e. +, -, or empty).
   *   - XYZ is a string of length N with one letter for each variant of a bound symbol,
   *     using `P` (positive variance), `N` (negative variance), `I` (invariant).
   *   - for each positive or negative variance v_i there is a parent trait Pj which
   *     is the same as LambdaXYZ except that it has `I` in i-th position.
   */
  def LambdaTrait(vcs: List[Int]): ClassSymbol = {
    assert(vcs.nonEmpty)

    def varianceFlags(v: Int) = v match {
      case -1 => Contravariant
      case  0 => EmptyFlags
      case  1 => Covariant
    }

    val completer = new LazyType  {
      def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
        val cls = denot.asClass.classSymbol
        val paramDecls = newScope
        for (i <- 0 until vcs.length)
          newTypeParam(cls, tpnme.LambdaArgName(i), varianceFlags(vcs(i)), paramDecls)
        newTypeField(cls, tpnme.hkApply, Covariant, paramDecls)
        val parentTraitRefs =
          for (i <- 0 until vcs.length if vcs(i) != 0)
          yield LambdaTrait(vcs.updated(i, 0)).typeRef
        denot.info = ClassInfo(
            ScalaPackageClass.thisType, cls, ObjectClass.typeRef :: parentTraitRefs.toList, paramDecls)
      }
    }

    val traitName = tpnme.LambdaTraitName(vcs)

    def createTrait = {
      val cls = newClassSymbol(
        ScalaPackageClass,
        traitName,
        PureInterfaceCreationFlags | Synthetic,
        completer)
      myLambdaTraits += cls
      cls
    }

    LambdaTraitForVariances.getOrElseUpdate(vcs, createTrait)
  }

  // ----- primitive value class machinery ------------------------------------------

  lazy val ScalaNumericValueTypeList = List(
    ByteTypeRef,
    ShortTypeRef,
    CharTypeRef,
    IntTypeRef,
    LongTypeRef,
    FloatTypeRef,
    DoubleTypeRef)

  lazy val ScalaNumericValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypeList.toSet
  def ScalaNumericValueClasses = ScalaNumericValueTypes.map(_.symbol)
  lazy val ScalaValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypes + UnitTypeRef + BooleanTypeRef
  def ScalaValueClasses = ScalaValueTypes.map(_.symbol)

  lazy val ScalaBoxedTypeRefs = ScalaValueTypes map (t => boxedTypeRef(t.name))
  def ScalaBoxedClasses = ScalaBoxedTypeRefs.map(_.symbol)

  private[this] val _boxedTypeRef = mutable.Map[TypeName, TypeRef]()
  private[this] val _unboxedTypeRef = mutable.Map[TypeName, TypeRef]()

  private[this] val _javaTypeToValueTypeRef = mutable.Map[Class[_], TypeRef]()
  private[this] val _valueTypeRefToJavaType = mutable.Map[TypeName, Class[_]]()
  private[this] val _valueTypeEnc = mutable.Map[TypeName, PrimitiveClassEnc]()

  val boxedTypeRef: collection.Map[TypeName, TypeRef] = _boxedTypeRef
  val unboxedTypeRef: collection.Map[TypeName, TypeRef] = _unboxedTypeRef
  val javaTypeToValueTypeRef: collection.Map[Class[_], TypeRef] = _javaTypeToValueTypeRef
  val valueTypeRefToJavaType: collection.Map[TypeName, Class[_]] = _valueTypeRefToJavaType
  val valueTypeEnc: collection.Map[TypeName, Int] = _valueTypeEnc

  private def valueTypeRef(name: String, boxed: TypeRef, jtype: Class[_], enc: Int): TypeRef = {
    val vcls = ctx.requiredClassRef(name)
    _unboxedTypeRef(boxed.name) = vcls
    _boxedTypeRef(vcls.name) = boxed
    _javaTypeToValueTypeRef(jtype) = vcls
    _valueTypeRefToJavaType(vcls.name) = jtype
    _valueTypeEnc(vcls.name) = enc
    vcls
  }

  /** The classes for which a Ref type exists. */
  lazy val refTypeKeys: collection.Set[TypeRef] = ScalaNumericValueTypes + BooleanTypeRef + ObjectClass.typeRef

  lazy val refTypeRef: Map[TypeRef, TypeRef] =
    refTypeKeys.map(rc => rc -> ctx.requiredClassRef(s"scala.runtime.${rc.name}Ref")).toMap

  lazy val volatileRefTypeRef: Map[TypeRef, TypeRef] =
    refTypeKeys.map(rc => rc -> ctx.requiredClassRef(s"scala.runtime.Volatile${rc.name}Ref")).toMap

  lazy val boxedRefTypeRefs: collection.Set[TypeRef] =
    refTypeKeys.flatMap(k => Set(refTypeRef(k), volatileRefTypeRef(k)))

  def wrapArrayMethodName(elemtp: Type): TermName = {
    val cls = elemtp.classSymbol
    if (cls.isPrimitiveValueClass) nme.wrapXArray(cls.name)
    else if (cls.derivesFrom(ObjectClass) && !cls.isPhantomClass) nme.wrapRefArray
    else nme.genericWrapArray
  }

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
    isValueSubType(sym1.typeRef, sym2.typeRef)

  // ----- Initialization ---------------------------------------------------

  /** Lists core classes that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  lazy val syntheticCoreClasses = List(
    AnyClass,
    AnyRefAlias,
    RepeatedParamClass,
    ByNameParamClass2x,
    AnyValClass,
    NullClass,
    NothingClass,
    SingletonClass,
    EqualsPatternClass,
    EmptyPackageVal,
    OpsPackageClass)

    /** Lists core methods that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
    lazy val syntheticCoreMethods = AnyMethods ++ ObjectMethods ++ List(String_+)

  private[this] var _isInitialized = false
  def isInitialized = _isInitialized

  def init(implicit ctx: Context) = {
    this.ctx = ctx
    if (!_isInitialized) {
      // force initialization of every symbol that is synthesized or hijacked by the compiler
      val forced = syntheticCoreClasses ++ syntheticCoreMethods ++ ScalaValueClasses
      _isInitialized = true
    }
  }
}
