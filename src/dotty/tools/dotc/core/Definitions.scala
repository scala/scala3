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
import scala.reflect.api.{ Universe => ApiUniverse }

object Definitions {
  val MaxFunctionArity, MaxTupleArity = 22
  class SymbolPerRun(ref: NamedType) {
    def symbol(implicit ctx: Context): Symbol = ref.symbol
  }
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

  private def requiredModule(path: String) = new SymbolPerRun(ctx.requiredModuleRef(path))

  private def requiredClass(path: String) = {
    val ref = ctx.requiredClassRef(path)
    (new SymbolPerRun(ref), ref)
  }

  private def requiredMethod(cls: Symbol, name: PreName) =
    new SymbolPerRun(cls.requiredMethodRef(name))

  private def requiredMethod(cls: Symbol, name: PreName, args: List[Type]) =
    new SymbolPerRun(cls.requiredMethodRef(name, args))

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
    lazy val Sys_error = requiredMethod(SysPackage.moduleClass, nme.error)

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
  def AnyType = AnyClass.typeRef
  lazy val AnyValClass: ClassSymbol = completeClass(newCompleteClassSymbol(ScalaPackageClass, tpnme.AnyVal, Abstract, List(AnyClass.typeRef)))
  def AnyValType = AnyValClass.typeRef

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
  def ObjectType = ObjectClass.typeRef

  lazy val AnyRefAlias: TypeSymbol = newAliasType(tpnme.AnyRef, ObjectType)
  def AnyRefType = AnyRefAlias.typeRef

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
      pt => MethodType(List(FunctionOf(Nil, PolyParam(pt, 0))), PolyParam(pt, 0)))

  /** Method representing a throw */
  lazy val throwMethod = newMethod(OpsPackageClass, nme.THROWkw,
      MethodType(List(ThrowableType), NothingType))

  lazy val NothingClass: ClassSymbol = newCompleteClassSymbol(
    ScalaPackageClass, tpnme.Nothing, AbstractFinal, List(AnyClass.typeRef))
  def NothingType = NothingClass.typeRef
  lazy val NullClass: ClassSymbol = newCompleteClassSymbol(
    ScalaPackageClass, tpnme.Null, AbstractFinal, List(ObjectClass.typeRef))
  def NullType = NullClass.typeRef

  lazy val ScalaPredefModuleRef = ctx.requiredModuleRef("scala.Predef")
  def ScalaPredefModule = ScalaPredefModuleRef.symbol
    lazy val Predef_conforms = requiredMethod(ScalaPredefModule, "$conforms")

  lazy val ScalaRuntimeModuleRef = ctx.requiredModuleRef("scala.runtime.ScalaRunTime")
  def ScalaRuntimeModule = ScalaRuntimeModuleRef.symbol
  def ScalaRuntimeClass = ScalaRuntimeModule.moduleClass.asClass

    def runtimeMethodRef(name: PreName) = ScalaRuntimeModule.requiredMethodRef(name)
    def ScalaRuntime_dropR = runtimeMethodRef(nme.drop)
    def ScalaRuntime_drop = ScalaRuntime_dropR.symbol

  lazy val BoxesRunTimeModuleRef = ctx.requiredModuleRef("scala.runtime.BoxesRunTime")
  def BoxesRunTimeModule = BoxesRunTimeModuleRef.symbol
  def BoxesRunTimeClass = BoxesRunTimeModule.moduleClass.asClass
  lazy val ScalaStaticsModuleRef = ctx.requiredModuleRef("scala.runtime.Statics")
  def ScalaStaticsModule = ScalaStaticsModuleRef.symbol
  def ScalaStaticsClass = ScalaStaticsModule.moduleClass.asClass

    def staticsMethodRef(name: PreName) = ScalaStaticsModule.requiredMethodRef(name)
    def staticsMethod(name: PreName) = ScalaStaticsModule.requiredMethod(name)

  lazy val DottyPredefModuleRef = ctx.requiredModuleRef("dotty.DottyPredef")
  def DottyPredefModule = DottyPredefModuleRef.symbol
  lazy val DottyArraysModuleRef = ctx.requiredModuleRef("dotty.runtime.Arrays")
  def DottyArraysModule = DottyArraysModuleRef.symbol

    def newRefArrayMethod = DottyArraysModule.requiredMethod("newRefArray")

  lazy val NilModuleRef = ctx.requiredModuleRef("scala.collection.immutable.Nil")
  def NilModule = NilModuleRef.symbol

  lazy val SingletonClass: ClassSymbol =
    // needed as a synthetic class because Scala 2.x refers to it in classfiles
    // but does not define it as an explicit class.
    newCompleteClassSymbol(
      ScalaPackageClass, tpnme.Singleton, PureInterfaceCreationFlags | Final,
      List(AnyClass.typeRef), EmptyScope)

  lazy val SeqType: TypeRef = ctx.requiredClassRef("scala.collection.Seq")
  def SeqClass = SeqType.symbol.asClass
    lazy val Seq_apply = requiredMethod(SeqClass, nme.apply)
    lazy val Seq_head = requiredMethod(SeqClass, nme.head)

  lazy val ArrayType: TypeRef = ctx.requiredClassRef("scala.Array")
  def ArrayClass = ArrayType.symbol.asClass
    lazy val Array_apply = requiredMethod(ArrayClass, nme.apply)
    lazy val Array_update = requiredMethod(ArrayClass, nme.update)
    lazy val Array_length = requiredMethod(ArrayClass, nme.length)
    lazy val Array_clone = requiredMethod(ArrayClass, nme.clone_)
    lazy val ArrayConstructor = requiredMethod(ArrayClass, nme.CONSTRUCTOR)

  lazy val UnitType: TypeRef = valueTypeRef("scala.Unit", BoxedUnitType, java.lang.Void.TYPE, UnitEnc)
  def UnitClass = UnitType.symbol.asClass
  lazy val BooleanType = valueTypeRef("scala.Boolean", BoxedBooleanType, java.lang.Boolean.TYPE, BooleanEnc)
  def BooleanClass = BooleanType.symbol.asClass
    lazy val Boolean_! = requiredMethod(BooleanClass, nme.UNARY_!)
    lazy val Boolean_&& = requiredMethod(BooleanClass, nme.ZAND) // ### harmonize required... calls
    lazy val Boolean_|| = requiredMethod(BooleanClass, nme.ZOR)

  lazy val ByteType: TypeRef = valueTypeRef("scala.Byte", BoxedByteType, java.lang.Byte.TYPE, ByteEnc)
  def ByteClass = ByteType.symbol.asClass
  lazy val ShortType: TypeRef = valueTypeRef("scala.Short", BoxedShortType, java.lang.Short.TYPE, ShortEnc)
  def ShortClass = ShortType.symbol.asClass
  lazy val CharType: TypeRef = valueTypeRef("scala.Char", BoxedCharType, java.lang.Character.TYPE, CharEnc)
  def CharClass = CharType.symbol.asClass
  lazy val IntType: TypeRef = valueTypeRef("scala.Int", BoxedIntType, java.lang.Integer.TYPE, IntEnc)
  def IntClass = IntType.symbol.asClass
    lazy val Int_- = requiredMethod(IntClass, nme.MINUS, List(IntType))
    lazy val Int_+ = requiredMethod(IntClass, nme.PLUS, List(IntType))
    lazy val Int_/ = requiredMethod(IntClass, nme.DIV, List(IntType))
    lazy val Int_* = requiredMethod(IntClass, nme.MUL, List(IntType))
    lazy val Int_== = requiredMethod(IntClass, nme.EQ, List(IntType))
    lazy val Int_>= = requiredMethod(IntClass, nme.GE, List(IntType))
    lazy val Int_<= = requiredMethod(IntClass, nme.LE, List(IntType))
  lazy val LongType: TypeRef = valueTypeRef("scala.Long", BoxedLongType, java.lang.Long.TYPE, LongEnc)
  def LongClass = LongType.symbol.asClass
    lazy val Long_XOR_Long = LongType.member(nme.XOR).requiredSymbol(
      x => (x is Method) && (x.info.firstParamTypes.head isRef defn.LongClass)
    )
    lazy val Long_LSR_Int = LongType.member(nme.LSR).requiredSymbol(
      x => (x is Method) && (x.info.firstParamTypes.head isRef defn.IntClass)
    )
  lazy val FloatType: TypeRef = valueTypeRef("scala.Float", BoxedFloatType, java.lang.Float.TYPE, FloatEnc)
  def FloatClass = FloatType.symbol.asClass
  lazy val DoubleType: TypeRef = valueTypeRef("scala.Double", BoxedDoubleType, java.lang.Double.TYPE, DoubleEnc)
  def DoubleClass = DoubleType.symbol.asClass

  lazy val BoxedUnitType: TypeRef = ctx.requiredClassRef("scala.runtime.BoxedUnit")
  def BoxedUnitClass = BoxedUnitType.symbol.asClass

    def BoxedUnit_UNIT = BoxedUnitClass.linkedClass.requiredValue("UNIT")

  lazy val BoxedBooleanType: TypeRef = ctx.requiredClassRef("java.lang.Boolean")
  def BoxedBooleanClass = BoxedBooleanType.symbol.asClass
  lazy val BoxedByteType: TypeRef = ctx.requiredClassRef("java.lang.Byte")
  def BoxedByteClass = BoxedByteType.symbol.asClass
  lazy val BoxedShortType: TypeRef = ctx.requiredClassRef("java.lang.Short")
  def BoxedShortClass = BoxedShortType.symbol.asClass
  lazy val BoxedCharType: TypeRef = ctx.requiredClassRef("java.lang.Character")
  def BoxedCharClass = BoxedCharType.symbol.asClass
  lazy val BoxedIntType: TypeRef = ctx.requiredClassRef("java.lang.Integer")
  def BoxedIntClass = BoxedIntType.symbol.asClass
  lazy val BoxedLongType: TypeRef = ctx.requiredClassRef("java.lang.Long")
  def BoxedLongClass = BoxedLongType.symbol.asClass
  lazy val BoxedFloatType: TypeRef = ctx.requiredClassRef("java.lang.Float")
  def BoxedFloatClass = BoxedFloatType.symbol.asClass
  lazy val BoxedDoubleType: TypeRef = ctx.requiredClassRef("java.lang.Double")
  def BoxedDoubleClass = BoxedDoubleType.symbol.asClass

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
  def StringType: Type                = StringClass.typeRef
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

  lazy val SerializableType: TypeRef       = ctx.requiredClassRef("scala.Serializable")
  def SerializableClass = SerializableType.symbol.asClass
  lazy val StringBuilderType: TypeRef      = ctx.requiredClassRef("scala.collection.mutable.StringBuilder")
  def StringBuilderClass = StringBuilderType.symbol.asClass
  lazy val MatchErrorType: TypeRef         = ctx.requiredClassRef("scala.MatchError")
  def MatchErrorClass = MatchErrorType.symbol.asClass

  lazy val StringAddType: TypeRef          = ctx.requiredClassRef("scala.runtime.StringAdd")
  def StringAddClass = StringAddType.symbol.asClass
    lazy val StringAdd_+ = requiredMethod(StringAddClass, nme.raw.PLUS)

  lazy val PairType: TypeRef                    = ctx.requiredClassRef("dotty.Pair")
  def PairClass = PairType.symbol.asClass
  lazy val PartialFunctionType: TypeRef         = ctx.requiredClassRef("scala.PartialFunction")
  def PartialFunctionClass = PartialFunctionType.symbol.asClass
  lazy val AbstractPartialFunctionType: TypeRef = ctx.requiredClassRef("scala.runtime.AbstractPartialFunction")
  def AbstractPartialFunctionClass = AbstractPartialFunctionType.symbol.asClass
  lazy val SymbolType: TypeRef                  = ctx.requiredClassRef("scala.Symbol")
  def SymbolClass = SymbolType.symbol.asClass
  lazy val DynamicType: TypeRef                 = ctx.requiredClassRef("scala.Dynamic")
  def DynamicClass = DynamicType.symbol.asClass
  lazy val OptionType: TypeRef                  = ctx.requiredClassRef("scala.Option")
  def OptionClass = OptionType.symbol.asClass
  lazy val ProductType: TypeRef                 = ctx.requiredClassRef("scala.Product")
  def ProductClass = ProductType.symbol.asClass
    lazy val Product_canEqual = requiredMethod(ProductClass, nme.canEqual_)
    lazy val Product_productArity = requiredMethod(ProductClass, nme.productArity)
    lazy val Product_productPrefix = requiredMethod(ProductClass, nme.productPrefix)
  lazy val LanguageModuleRef          = ctx.requiredModule("dotty.language")
  def LanguageModuleClass = LanguageModuleRef.symbol.moduleClass.asClass
  lazy val NonLocalReturnControlType: TypeRef   = ctx.requiredClassRef("scala.runtime.NonLocalReturnControl")

  // Annotation base classes
  lazy val AnnotationType              = ctx.requiredClassRef("scala.annotation.Annotation")
  def AnnotationClass = AnnotationType.symbol.asClass
  lazy val ClassfileAnnotationType     = ctx.requiredClassRef("scala.annotation.ClassfileAnnotation")
  def ClassfileAnnotationClass = ClassfileAnnotationType.symbol.asClass
  lazy val StaticAnnotationType        = ctx.requiredClassRef("scala.annotation.StaticAnnotation")
  def StaticAnnotationClass = StaticAnnotationType.symbol.asClass

  // Annotation classes
  lazy val AliasAnnotType = ctx.requiredClassRef("dotty.annotation.internal.Alias")
  def AliasAnnot = AliasAnnotType.symbol.asClass
  lazy val AnnotationDefaultAnnotType = ctx.requiredClassRef("dotty.annotation.internal.AnnotationDefault")
  def AnnotationDefaultAnnot = AnnotationDefaultAnnotType.symbol.asClass
  lazy val ChildAnnotType = ctx.requiredClassRef("dotty.annotation.internal.Child")
  def ChildAnnot = ChildAnnotType.symbol.asClass
  lazy val CovariantBetweenAnnotType = ctx.requiredClassRef("dotty.annotation.internal.CovariantBetween")
  def CovariantBetweenAnnot = CovariantBetweenAnnotType.symbol.asClass
  lazy val ContravariantBetweenAnnotType = ctx.requiredClassRef("dotty.annotation.internal.ContravariantBetween")
  def ContravariantBetweenAnnot = ContravariantBetweenAnnotType.symbol.asClass
  lazy val DeprecatedAnnotType = ctx.requiredClassRef("scala.deprecated")
  def DeprecatedAnnot = DeprecatedAnnotType.symbol.asClass
  lazy val InvariantBetweenAnnotType = ctx.requiredClassRef("dotty.annotation.internal.InvariantBetween")
  def InvariantBetweenAnnot = InvariantBetweenAnnotType.symbol.asClass
  lazy val MigrationAnnotType = ctx.requiredClassRef("scala.annotation.migration")
  def MigrationAnnot = MigrationAnnotType.symbol.asClass
  lazy val NativeAnnotType                   = ctx.requiredClassRef("scala.native")
  def NativeAnnot = NativeAnnotType.symbol.asClass
  lazy val RemoteAnnotType                   = ctx.requiredClassRef("scala.remote")
  def RemoteAnnot = RemoteAnnotType.symbol.asClass
  lazy val RepeatedAnnotType = ctx.requiredClassRef("dotty.annotation.internal.Repeated")
  def RepeatedAnnot = RepeatedAnnotType.symbol.asClass
  lazy val ScalaSignatureAnnotType = ctx.requiredClassRef("scala.reflect.ScalaSignature")
  def ScalaSignatureAnnot = ScalaSignatureAnnotType.symbol.asClass
  lazy val ScalaLongSignatureAnnotType = ctx.requiredClassRef("scala.reflect.ScalaLongSignature")
  def ScalaLongSignatureAnnot = ScalaLongSignatureAnnotType.symbol.asClass
  lazy val ScalaStrictFPAnnotType            = ctx.requiredClassRef("scala.annotation.strictfp")
  def ScalaStrictFPAnnot = ScalaStrictFPAnnotType.symbol.asClass
  lazy val SerialVersionUIDAnnotType         = ctx.requiredClassRef("scala.SerialVersionUID")
  def SerialVersionUIDAnnot = SerialVersionUIDAnnotType.symbol.asClass
  lazy val TASTYSignatureAnnotType = ctx.requiredClassRef("scala.annotation.internal.TASTYSignature")
  def TASTYSignatureAnnot = TASTYSignatureAnnotType.symbol.asClass
  lazy val TASTYLongSignatureAnnotType = ctx.requiredClassRef("scala.annotation.internal.TASTYLongSignature")
  def TASTYLongSignatureAnnot = TASTYLongSignatureAnnotType.symbol.asClass
  lazy val TailrecAnnotType = ctx.requiredClassRef("scala.annotation.tailrec")
  def TailrecAnnot = TailrecAnnotType.symbol.asClass
  lazy val ThrowsAnnotType = ctx.requiredClassRef("scala.throws")
  def ThrowsAnnot = ThrowsAnnotType.symbol.asClass
  lazy val TransientAnnotType                = ctx.requiredClassRef("scala.transient")
  def TransientAnnot = TransientAnnotType.symbol.asClass
  lazy val UncheckedAnnotType = ctx.requiredClassRef("scala.unchecked")
  def UncheckedAnnot = UncheckedAnnotType.symbol.asClass
  lazy val UncheckedStableAnnotType = ctx.requiredClassRef("scala.annotation.unchecked.uncheckedStable")
  def UncheckedStableAnnot = UncheckedStableAnnotType.symbol.asClass
  lazy val UncheckedVarianceAnnotType = ctx.requiredClassRef("scala.annotation.unchecked.uncheckedVariance")
  def UncheckedVarianceAnnot = UncheckedVarianceAnnotType.symbol.asClass
  lazy val VolatileAnnotType = ctx.requiredClassRef("scala.volatile")
  def VolatileAnnot = VolatileAnnotType.symbol.asClass
  lazy val FieldMetaAnnotType = ctx.requiredClassRef("scala.annotation.meta.field")
  def FieldMetaAnnot = FieldMetaAnnotType.symbol.asClass
  lazy val GetterMetaAnnotType = ctx.requiredClassRef("scala.annotation.meta.getter")
  def GetterMetaAnnot = GetterMetaAnnotType.symbol.asClass
  lazy val SetterMetaAnnotType = ctx.requiredClassRef("scala.annotation.meta.setter")
  def SetterMetaAnnot = SetterMetaAnnotType.symbol.asClass

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
    def apply(args: List[Type], resultType: Type)(implicit ctx: Context) =
      FunctionType(args.length).appliedTo(args ::: resultType :: Nil)
    def unapply(ft: Type)(implicit ctx: Context)/*: Option[(List[Type], Type)]*/ = {
      // -language:keepUnions difference: unapply needs result type because inferred type
      // is Some[(List[Type], Type)] | None, which is not a legal unapply type.
      val tsym = ft.typeSymbol
      lazy val targs = ft.argInfos
      val numArgs = targs.length - 1
      if (numArgs >= 0 && numArgs <= MaxFunctionArity &&
          (FunctionType(numArgs).symbol == tsym)) Some(targs.init, targs.last)
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

  object MultiArrayOf {
    def apply(elem: Type, ndims: Int)(implicit ctx: Context): Type =
      if (ndims == 0) elem else ArrayOf(apply(elem, ndims - 1))
    def unapply(tp: Type)(implicit ctx: Context): Option[(Type, Int)] = tp match {
      case ArrayOf(elemtp) =>
        elemtp match {
          case MultiArrayOf(finalElemTp, n) => Some(finalElemTp, n + 1)
          case _ => Some(elemtp, 1)
        }
      case _ =>
        None
    }
  }

  // ----- Symbol sets ---------------------------------------------------

  lazy val AbstractFunctionType = mkArityArray("scala.runtime.AbstractFunction", MaxFunctionArity, 0)
  def AbstractFunctionClass = AbstractFunctionType.map(_.symbol.asClass)
  lazy val FunctionType = mkArityArray("scala.Function", MaxFunctionArity, 0)
  def FunctionClass = FunctionType.map(_.symbol.asClass)
    lazy val Function0_applyR = FunctionType(0).symbol.requiredMethodRef(nme.apply)
    def Function0_apply = Function0_applyR.symbol

  lazy val TupleType = mkArityArray("scala.Tuple", MaxTupleArity, 2)
  lazy val ProductNType = mkArityArray("scala.Product", MaxTupleArity, 0)

  private lazy val FunctionTypes: Set[TypeRef] = FunctionType.toSet
  private lazy val TupleTypes: Set[TypeRef] = TupleType.toSet
  private lazy val ProductTypes: Set[TypeRef] = ProductNType.toSet

  /** If `cls` is a class in the scala package, its name, otherwise EmptyTypeName */
  def scalaClassName(cls: Symbol)(implicit ctx: Context): TypeName =
    if (cls.isClass && cls.owner == ScalaPackageClass) cls.asClass.name else EmptyTypeName

  /** If type `ref` refers to a class in the scala package, its name, otherwise EmptyTypeName */
  def scalaClassName(ref: Type)(implicit ctx: Context): TypeName = scalaClassName(ref.classSymbol)

  private def isVarArityClass(cls: Symbol, prefix: Name) = {
    val name = scalaClassName(cls)
    name.startsWith(prefix) && name.drop(prefix.length).forall(_.isDigit)
  }

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

  def isPolymorphicAfterErasure(sym: Symbol) =
     (sym eq Any_isInstanceOf) || (sym eq Any_asInstanceOf) || (sym eq newRefArrayMethod)

  def isTupleType(tp: Type)(implicit ctx: Context) = {
    val arity = tp.dealias.argInfos.length
    arity <= MaxTupleArity && TupleType(arity) != null && (tp isRef TupleType(arity).symbol)
  }

  def tupleType(elems: List[Type]) = {
    TupleType(elems.size).appliedTo(elems)
  }

  def isProductSubType(tp: Type)(implicit ctx: Context) =
    (tp derivesFrom ProductType.symbol) && tp.baseClasses.exists(isProductClass)

  def isFunctionType(tp: Type)(implicit ctx: Context) = {
    val arity = functionArity(tp)
    0 <= arity && arity <= MaxFunctionArity && (tp isRef FunctionType(arity).symbol)
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

  class SymbolsPerRun(generate: Context => collection.Set[Symbol]) {
    private var current: RunId = NoRunId
    private var syms: collection.Set[Symbol] = _
    def apply()(implicit ctx: Context) = {
      if (current != ctx.runId) {
        syms = generate(ctx)
        current = ctx.runId
      }
      syms
    }
  }

  lazy val ScalaNumericValueTypeList = List(
    ByteType, ShortType, CharType, IntType, LongType, FloatType, DoubleType)

  private lazy val ScalaNumericValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypeList.toSet
  private lazy val ScalaValueTypes: collection.Set[TypeRef] = ScalaNumericValueTypes + UnitType + BooleanType
  private lazy val ScalaBoxedTypes = ScalaValueTypes map (t => boxedTypes(t.name))

  val ScalaNumericValueClasses = new SymbolsPerRun(implicit ctx => ScalaNumericValueTypes.map(_.symbol))
  val ScalaValueClasses        = new SymbolsPerRun(implicit ctx => ScalaValueTypes.map(_.symbol))
  val ScalaBoxedClasses        = new SymbolsPerRun(implicit ctx => ScalaBoxedTypes.map(_.symbol))

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
    valueTypeEnc(sym2.asClass.name) % valueTypeEnc(sym1.asClass.name) == 0

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
  private def isInitialized = _isInitialized

  def init(implicit ctx: Context) = {
    this.ctx = ctx
    if (!_isInitialized) {
      // force initialization of every symbol that is synthesized or hijacked by the compiler
      val forced = syntheticCoreClasses ++ syntheticCoreMethods ++ ScalaValueClasses()
      _isInitialized = true
    }
  }
}
