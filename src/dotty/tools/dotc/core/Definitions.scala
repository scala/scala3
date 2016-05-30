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

  // NOTE: Ideally we would write `parentConstrs: => Type*` but SIP-24 is only
  // implemented in Dotty and not in Scala 2.
  // See <http://docs.scala-lang.org/sips/pending/repeated-byname.html>.
  private def specialPolyClass(name: TypeName, paramFlags: FlagSet, parentConstrs: => Seq[Type]): ClassSymbol = {
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
    lazy val Sys_errorR = SysPackage.moduleClass.requiredMethodRef(nme.error)
    def Sys_error(implicit ctx: Context) = Sys_errorR.symbol

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
  def ScalaPredefModule(implicit ctx: Context) = ScalaPredefModuleRef.symbol

    lazy val Predef_conformsR = ScalaPredefModule.requiredMethodRef("$conforms")
    def Predef_conforms(implicit ctx: Context) = Predef_conformsR.symbol
    lazy val Predef_classOfR = ScalaPredefModule.requiredMethodRef("classOf")
    def Predef_classOf(implicit ctx: Context) = Predef_classOfR.symbol

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

  lazy val DottyPredefModuleRef = ctx.requiredModuleRef("dotty.DottyPredef")
  def DottyPredefModule(implicit ctx: Context) = DottyPredefModuleRef.symbol

    def Predef_eqAny(implicit ctx: Context) = DottyPredefModule.requiredMethod(nme.eqAny)

  lazy val DottyArraysModuleRef = ctx.requiredModuleRef("dotty.runtime.Arrays")
  def DottyArraysModule(implicit ctx: Context) = DottyArraysModuleRef.symbol
    def newGenericArrayMethod(implicit ctx: Context) = DottyArraysModule.requiredMethod("newGenericArray")
    def newArrayMethod(implicit ctx: Context) = DottyArraysModule.requiredMethod("newArray")

  lazy val NilModuleRef = ctx.requiredModuleRef("scala.collection.immutable.Nil")
  def NilModule(implicit ctx: Context) = NilModuleRef.symbol

  lazy val SingletonClass: ClassSymbol =
    // needed as a synthetic class because Scala 2.x refers to it in classfiles
    // but does not define it as an explicit class.
    newCompleteClassSymbol(
      ScalaPackageClass, tpnme.Singleton, PureInterfaceCreationFlags | Final,
      List(AnyClass.typeRef), EmptyScope)

  lazy val SeqType: TypeRef = ctx.requiredClassRef("scala.collection.Seq")
  def SeqClass(implicit ctx: Context) = SeqType.symbol.asClass

    lazy val Seq_applyR = SeqClass.requiredMethodRef(nme.apply)
    def Seq_apply(implicit ctx: Context) = Seq_applyR.symbol
    lazy val Seq_headR = SeqClass.requiredMethodRef(nme.head)
    def Seq_head(implicit ctx: Context) = Seq_headR.symbol

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

  lazy val ByNameParamClass2x = specialPolyClass(tpnme.BYNAME_PARAM_CLASS, Covariant, Seq(AnyType))
  lazy val EqualsPatternClass = specialPolyClass(tpnme.EQUALS_PATTERN, EmptyFlags, Seq(AnyType))

  lazy val RepeatedParamClass = specialPolyClass(tpnme.REPEATED_PARAM_CLASS, Covariant, Seq(ObjectType, SeqType))

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
  def SerializableClass(implicit ctx: Context) = SerializableType.symbol.asClass
  lazy val StringBuilderType: TypeRef      = ctx.requiredClassRef("scala.collection.mutable.StringBuilder")
  def StringBuilderClass(implicit ctx: Context) = StringBuilderType.symbol.asClass
  lazy val MatchErrorType: TypeRef         = ctx.requiredClassRef("scala.MatchError")
  def MatchErrorClass(implicit ctx: Context) = MatchErrorType.symbol.asClass

  lazy val StringAddType: TypeRef          = ctx.requiredClassRef("scala.runtime.StringAdd")
  def StringAddClass(implicit ctx: Context) = StringAddType.symbol.asClass

    lazy val StringAdd_plusR = StringAddClass.requiredMethodRef(nme.raw.PLUS)
    def StringAdd_+(implicit ctx: Context) = StringAdd_plusR.symbol

  lazy val PairType: TypeRef                    = ctx.requiredClassRef("dotty.Pair")
  def PairClass(implicit ctx: Context) = PairType.symbol.asClass
  lazy val PartialFunctionType: TypeRef         = ctx.requiredClassRef("scala.PartialFunction")
  def PartialFunctionClass(implicit ctx: Context) = PartialFunctionType.symbol.asClass
  lazy val AbstractPartialFunctionType: TypeRef = ctx.requiredClassRef("scala.runtime.AbstractPartialFunction")
  def AbstractPartialFunctionClass(implicit ctx: Context) = AbstractPartialFunctionType.symbol.asClass
  lazy val SymbolType: TypeRef                  = ctx.requiredClassRef("scala.Symbol")
  def SymbolClass(implicit ctx: Context) = SymbolType.symbol.asClass
  lazy val DynamicType: TypeRef                 = ctx.requiredClassRef("scala.Dynamic")
  def DynamicClass(implicit ctx: Context) = DynamicType.symbol.asClass
  lazy val OptionType: TypeRef                  = ctx.requiredClassRef("scala.Option")
  def OptionClass(implicit ctx: Context) = OptionType.symbol.asClass
  lazy val ProductType: TypeRef                 = ctx.requiredClassRef("scala.Product")
  def ProductClass(implicit ctx: Context) = ProductType.symbol.asClass
    lazy val Product_canEqualR = ProductClass.requiredMethodRef(nme.canEqual_)
    def Product_canEqual(implicit ctx: Context) = Product_canEqualR.symbol
    lazy val Product_productArityR = ProductClass.requiredMethodRef(nme.productArity)
    def Product_productArity(implicit ctx: Context) = Product_productArityR.symbol
    lazy val Product_productPrefixR = ProductClass.requiredMethodRef(nme.productPrefix)
    def Product_productPrefix(implicit ctx: Context) = Product_productPrefixR.symbol
  lazy val LanguageModuleRef          = ctx.requiredModule("dotty.language")
  def LanguageModuleClass(implicit ctx: Context) = LanguageModuleRef.symbol.moduleClass.asClass
  lazy val NonLocalReturnControlType: TypeRef   = ctx.requiredClassRef("scala.runtime.NonLocalReturnControl")

  lazy val ClassTagType = ctx.requiredClassRef("scala.reflect.ClassTag")
  def ClassTagClass(implicit ctx: Context) = ClassTagType.symbol.asClass
  def ClassTagModule(implicit ctx: Context) = ClassTagClass.companionModule

  lazy val EqType = ctx.requiredClassRef("scala.Eq")
  def EqClass(implicit ctx: Context) = EqType.symbol.asClass

  // Annotation base classes
  lazy val AnnotationType              = ctx.requiredClassRef("scala.annotation.Annotation")
  def AnnotationClass(implicit ctx: Context) = AnnotationType.symbol.asClass
  lazy val ClassfileAnnotationType     = ctx.requiredClassRef("scala.annotation.ClassfileAnnotation")
  def ClassfileAnnotationClass(implicit ctx: Context) = ClassfileAnnotationType.symbol.asClass
  lazy val StaticAnnotationType        = ctx.requiredClassRef("scala.annotation.StaticAnnotation")
  def StaticAnnotationClass(implicit ctx: Context) = StaticAnnotationType.symbol.asClass

  // Annotation classes
  lazy val AliasAnnotType = ctx.requiredClassRef("dotty.annotation.internal.Alias")
  def AliasAnnot(implicit ctx: Context) = AliasAnnotType.symbol.asClass
  lazy val AnnotationDefaultAnnotType = ctx.requiredClassRef("dotty.annotation.internal.AnnotationDefault")
  def AnnotationDefaultAnnot(implicit ctx: Context) = AnnotationDefaultAnnotType.symbol.asClass
  lazy val ChildAnnotType = ctx.requiredClassRef("dotty.annotation.internal.Child")
  def ChildAnnot(implicit ctx: Context) = ChildAnnotType.symbol.asClass
  lazy val CovariantBetweenAnnotType = ctx.requiredClassRef("dotty.annotation.internal.CovariantBetween")
  def CovariantBetweenAnnot(implicit ctx: Context) = CovariantBetweenAnnotType.symbol.asClass
  lazy val ContravariantBetweenAnnotType = ctx.requiredClassRef("dotty.annotation.internal.ContravariantBetween")
  def ContravariantBetweenAnnot(implicit ctx: Context) = ContravariantBetweenAnnotType.symbol.asClass
  lazy val DeprecatedAnnotType = ctx.requiredClassRef("scala.deprecated")
  def DeprecatedAnnot(implicit ctx: Context) = DeprecatedAnnotType.symbol.asClass
  lazy val ImplicitNotFoundAnnotType = ctx.requiredClassRef("scala.annotation.implicitNotFound")
  def ImplicitNotFoundAnnot(implicit ctx: Context) = ImplicitNotFoundAnnotType.symbol.asClass
  lazy val InvariantBetweenAnnotType = ctx.requiredClassRef("dotty.annotation.internal.InvariantBetween")
  def InvariantBetweenAnnot(implicit ctx: Context) = InvariantBetweenAnnotType.symbol.asClass
  lazy val MigrationAnnotType = ctx.requiredClassRef("scala.annotation.migration")
  def MigrationAnnot(implicit ctx: Context) = MigrationAnnotType.symbol.asClass
  lazy val NativeAnnotType                   = ctx.requiredClassRef("scala.native")
  def NativeAnnot(implicit ctx: Context) = NativeAnnotType.symbol.asClass
  lazy val RemoteAnnotType                   = ctx.requiredClassRef("scala.remote")
  def RemoteAnnot(implicit ctx: Context) = RemoteAnnotType.symbol.asClass
  lazy val RepeatedAnnotType = ctx.requiredClassRef("dotty.annotation.internal.Repeated")
  def RepeatedAnnot(implicit ctx: Context) = RepeatedAnnotType.symbol.asClass
  lazy val SourceFileAnnotType = ctx.requiredClassRef("dotty.annotation.internal.SourceFile")
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
  lazy val UnsafeNonvariantAnnotType = ctx.requiredClassRef("dotty.annotation.internal.UnsafeNonvariant")
  def UnsafeNonvariantAnnot(implicit ctx: Context) = UnsafeNonvariantAnnotType.symbol.asClass
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

  lazy val AbstractFunctionType = mkArityArray("scala.runtime.AbstractFunction", MaxFunctionArity, 0)
  val AbstractFunctionClassPerRun = new PerRun[Array[Symbol]](implicit ctx => AbstractFunctionType.map(_.symbol.asClass))
  def AbstractFunctionClass(n: Int)(implicit ctx: Context) = AbstractFunctionClassPerRun()(ctx)(n)
  lazy val FunctionType = mkArityArray("scala.Function", MaxFunctionArity, 0)
  def FunctionClassPerRun = new PerRun[Array[Symbol]](implicit ctx => FunctionType.map(_.symbol.asClass))
  def FunctionClass(n: Int)(implicit ctx: Context) = FunctionClassPerRun()(ctx)(n)
    lazy val Function0_applyR = FunctionType(0).symbol.requiredMethodRef(nme.apply)
    def Function0_apply(implicit ctx: Context) = Function0_applyR.symbol

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

  def isBottomClass(cls: Symbol) = cls == NothingClass || cls == NullClass
  def isBottomType(tp: Type) = {
    def test(implicit ctx: Context) = tp.derivesFrom(NothingClass) || tp.derivesFrom(NullClass)
    try test
    catch { // See remark in SymDenotations#accessWithin
      case ex: NotDefinedHere => test(ctx.addMode(Mode.FutureDefsOK))
    }
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
     (sym eq Any_isInstanceOf) || (sym eq Any_asInstanceOf)

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
  def lambdaTraitsOBS: Set[Symbol] = myLambdaTraits

  private var LambdaTraitForVariances = mutable.Map[List[Int], ClassSymbol]()

  /** The HigherKinded trait corresponding to symbols `boundSyms` (which are assumed
   *  to be the type parameters of a higher-kided type). This is a class symbol that
   *  would be generated by the following schema.
   *
   *      trait LambdaXYZ extends Object with P1 with ... with Pn {
   *        type v_1 hk$0; ...; type v_N hk$N;
   *        type +$Apply
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
  def LambdaTraitOBS(vcs: List[Int]): ClassSymbol = {
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
          newTypeParam(cls, tpnme.hkArg(i), varianceFlags(vcs(i)), paramDecls)
        newTypeField(cls, tpnme.hkApplyOBS, Covariant, paramDecls)
        val parentTraitRefs =
          for (i <- 0 until vcs.length if vcs(i) != 0)
          yield LambdaTraitOBS(vcs.updated(i, 0)).typeRef
        denot.info = ClassInfo(
            ScalaPackageClass.thisType, cls, ObjectClass.typeRef :: parentTraitRefs.toList, paramDecls)
      }
    }

    val traitName = tpnme.hkLambdaOBS(vcs)

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
    lazy val syntheticCoreMethods = AnyMethods ++ ObjectMethods ++ List(String_+, throwMethod)

  private[this] var _isInitialized = false
  private def isInitialized = _isInitialized

  def init()(implicit ctx: Context) = {
    this.ctx = ctx
    if (!_isInitialized) {
      // force initialization of every symbol that is synthesized or hijacked by the compiler
      val forced = syntheticCoreClasses ++ syntheticCoreMethods ++ ScalaValueClasses()
      _isInitialized = true
    }
  }
}
