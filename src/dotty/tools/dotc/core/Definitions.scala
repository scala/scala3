package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Denotations._, SymDenotations._, StdNames._, Names._
import Flags._, Scopes._, Decorators._, NameOps._, util.Positions._
import pickling.UnPickler.ensureConstructor
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

  private def newTypeParam(cls: ClassSymbol, name: TypeName, flags: FlagSet, scope: MutableScope) =
    scope.enter(newSymbol(cls, name, flags | TypeParamCreationFlags, TypeBounds.empty))

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
    ScalaPackageClass.preDecls.enter(sym)
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
    newPolyMethod(cls, name, 1, pt => MethodType(Nil, Nil, resultTypeFn(pt)), flags)

  private def mkArityArray(name: String, arity: Int, countFrom: Int): Array[ClassSymbol] = {
    val arr = new Array[ClassSymbol](arity)
    for (i <- countFrom until arity) arr(i) = ctx.requiredClass("scala." + name + i)
    arr
  }

  lazy val RootClass: ClassSymbol = ctx.newPackageSymbol(
    NoSymbol, nme.ROOT, (root, rootcls) => ctx.rootLoader(root)).moduleClass.asClass
  lazy val RootPackage: TermSymbol = ctx.newSymbol(
    NoSymbol, nme.ROOTPKG, PackageCreationFlags, TypeRef(NoPrefix, RootClass))

  lazy val EmptyPackageClass = ctx.newCompletePackageSymbol(RootClass, nme.EMPTY_PACKAGE).moduleClass.asClass
  lazy val EmptyPackageVal = EmptyPackageClass.sourceModule.entered

  lazy val ScalaPackageVal = ctx.requiredPackage("scala")
  lazy val ScalaPackageClass = ScalaPackageVal.moduleClass.asClass
  lazy val JavaPackageVal = ctx.requiredPackage("java")
  lazy val JavaLangPackageVal = ctx.requiredPackage("java.lang")

  lazy val ObjectClass = ctx.requiredClass("java.lang.Object")
  lazy val AnyRefAlias: TypeSymbol = newAliasType(tpnme.AnyRef, ObjectType)

    lazy val Object_## = newMethod(ObjectClass, nme.HASHHASH, ExprType(IntType), Final)
    lazy val Object_== = newMethod(ObjectClass, nme.EQ, methOfAny(BooleanType), Final)
    lazy val Object_!= = newMethod(ObjectClass, nme.NE, methOfAny(BooleanType), Final)
    lazy val Object_eq = newMethod(ObjectClass, nme.eq, methOfAnyRef(BooleanType), Final)
    lazy val Object_ne = newMethod(ObjectClass, nme.ne, methOfAnyRef(BooleanType), Final)
    lazy val Object_isInstanceOf = newT1ParameterlessMethod(ObjectClass, nme.isInstanceOf_Ob, _ => BooleanType, Final | Synthetic)
    lazy val Object_asInstanceOf = newT1ParameterlessMethod(ObjectClass, nme.asInstanceOf_Ob, PolyParam(_, 0), Final | Synthetic)
    lazy val Object_synchronized = newPolyMethod(ObjectClass, nme.synchronized_, 1,
        pt => MethodType(List(PolyParam(pt, 0)), PolyParam(pt, 0)), Final)

    def Object_getClass  = objMethod(nme.getClass_)
    def Object_clone     = objMethod(nme.clone_)
    def Object_finalize  = objMethod(nme.finalize_)
    def Object_notify    = objMethod(nme.notify_)
    def Object_notifyAll = objMethod(nme.notifyAll_)
    def Object_equals    = objMethod(nme.equals_)
    def Object_hashCode  = objMethod(nme.hashCode_)
    def Object_toString  = objMethod(nme.toString_)
    private def objMethod(name: PreName) = ObjectClass.requiredMethod(name)

  lazy val AnyClass: ClassSymbol = {
    val cls = newCompleteClassSymbol(ScalaPackageClass, tpnme.Any, Abstract, Nil)
    ensureConstructor(cls, EmptyScope)
    cls
  }

  lazy val AnyValClass: ClassSymbol = ctx.requiredClass("scala.AnyVal")

    lazy val Any_==       = newMethod(AnyClass, nme.EQ, methOfAny(BooleanType), Final)
    lazy val Any_!=       = newMethod(AnyClass, nme.NE, methOfAny(BooleanType), Final)
    lazy val Any_equals   = newMethod(AnyClass, nme.equals_, methOfAny(BooleanType))
    lazy val Any_hashCode = newMethod(AnyClass, nme.hashCode_, ExprType(IntType))
    lazy val Any_toString = newMethod(AnyClass, nme.toString_, ExprType(StringType))
    lazy val Any_##       = newMethod(AnyClass, nme.HASHHASH, ExprType(IntType), Final)

    // Any_getClass requires special handling.  The return type is determined on
    // a per-call-site basis as if the function being called were actually:
    //
    //    // Assuming `target.getClass()`
    //    def getClass[T](target: T): Class[_ <: T]
    //
    // Since getClass is not actually a polymorphic method, this requires compiler
    // participation.  At the "Any" level, the return type is Class[_] as it is in
    // java.lang.Object.  Java also special cases the return type.
    lazy val Any_getClass     = newMethod(AnyClass, nme.getClass_, ExprType(Object_getClass.info.resultType), Deferred)
    lazy val Any_isInstanceOf = newT1ParameterlessMethod(AnyClass, nme.isInstanceOf_, _ => BooleanType, Final)
    lazy val Any_asInstanceOf = newT1ParameterlessMethod(AnyClass, nme.asInstanceOf_, PolyParam(_, 0), Final)

  lazy val NotNullClass = ctx.requiredClass("scala.NotNull")

  lazy val NothingClass: ClassSymbol = newCompleteClassSymbol(
    ScalaPackageClass, tpnme.Nothing, AbstractFinal, List(AnyClass.typeRef))
  lazy val NullClass: ClassSymbol = newCompleteClassSymbol(
    ScalaPackageClass, tpnme.Null, AbstractFinal, List(ObjectClass.typeRef))

  lazy val ScalaPredefModule = ctx.requiredModule("scala.Predef")
  lazy val DottyPredefModule = ctx.requiredModule("dotty.DottyPredef")
  lazy val NilModule = ctx.requiredModule("scala.collection.immutable.Nil")

//  lazy val FunctionClass: ClassSymbol = ctx.requiredClass("scala.Function")
  lazy val SingletonClass: ClassSymbol =
    // needed as a synthetic class because Scala 2.x refers to it in classfiles
    // but does not define it as an explicit class.
    newCompleteClassSymbol(
      ScalaPackageClass, tpnme.Singleton, Trait | Interface | Final,
      List(AnyClass.typeRef), EmptyScope)
  lazy val SeqClass: ClassSymbol = ctx.requiredClass("scala.collection.Seq")
  lazy val ArrayClass: ClassSymbol = ctx.requiredClass("scala.Array")
  lazy val uncheckedStableClass: ClassSymbol = ctx.requiredClass("scala.annotation.unchecked.uncheckedStable")

  lazy val UnitClass = valueClassSymbol("scala.Unit", BoxedUnitClass, java.lang.Void.TYPE, UnitEnc)
  lazy val BooleanClass = valueClassSymbol("scala.Boolean", BoxedBooleanClass, java.lang.Boolean.TYPE, BooleanEnc)

    lazy val Boolean_and = BooleanClass.requiredMethod(nme.ZAND)

  lazy val ByteClass = valueClassSymbol("scala.Byte", BoxedByteClass, java.lang.Byte.TYPE, ByteEnc)
  lazy val ShortClass = valueClassSymbol("scala.Short", BoxedShortClass, java.lang.Short.TYPE, ShortEnc)
  lazy val CharClass = valueClassSymbol("scala.Char", BoxedCharClass, java.lang.Character.TYPE, CharEnc)
  lazy val IntClass = valueClassSymbol("scala.Int", BoxedIntClass, java.lang.Integer.TYPE, IntEnc)
  lazy val LongClass = valueClassSymbol("scala.Long", BoxedLongClass, java.lang.Long.TYPE, LongEnc)
  lazy val FloatClass = valueClassSymbol("scala.Float", BoxedFloatClass, java.lang.Float.TYPE, FloatEnc)
  lazy val DoubleClass = valueClassSymbol("scala.Double", BoxedDoubleClass, java.lang.Double.TYPE, DoubleEnc)

  lazy val BoxedUnitClass = ctx.requiredClass("scala.runtime.BoxedUnit")

    lazy val BoxedUnit_UNIT = BoxedUnitClass.linkedClass.requiredValue("UNIT")

  lazy val BoxedBooleanClass = ctx.requiredClass("java.lang.Boolean")
  lazy val BoxedByteClass = ctx.requiredClass("java.lang.Byte")
  lazy val BoxedShortClass = ctx.requiredClass("java.lang.Short")
  lazy val BoxedCharClass = ctx.requiredClass("java.lang.Character")
  lazy val BoxedIntClass = ctx.requiredClass("java.lang.Integer")
  lazy val BoxedLongClass = ctx.requiredClass("java.lang.Long")
  lazy val BoxedFloatClass = ctx.requiredClass("java.lang.Float")
  lazy val BoxedDoubleClass = ctx.requiredClass("java.lang.Double")

  lazy val ByNameParamClass2x     = specialPolyClass(tpnme.BYNAME_PARAM_CLASS, Covariant, AnyType)
  lazy val EqualsPatternClass     = specialPolyClass(tpnme.EQUALS_PATTERN, EmptyFlags, AnyType)

  lazy val RepeatedParamClass     = specialPolyClass(tpnme.REPEATED_PARAM_CLASS, Covariant, SeqType)
  lazy val JavaRepeatedParamClass = specialPolyClass(tpnme.JAVA_REPEATED_PARAM_CLASS, Covariant, ArrayClass.typeRef)

  // fundamental classes
  lazy val StringClass                  = ctx.requiredClass("java.lang.String")

    lazy val String_+ = newMethod(StringClass, nme.raw.PLUS, methOfAny(StringType), Final)

  lazy val StringAddClass               = ctx.requiredClass("scala.runtime.StringAdd")

    lazy val StringAdd_+ = StringAddClass.requiredMethod(nme.raw.PLUS)

  lazy val PairClass                    = ctx.requiredClass("dotty.Pair")
  lazy val PartialFunctionClass         = ctx.requiredClass("scala.PartialFunction")
  lazy val AbstractPartialFunctionClass = ctx.requiredClass("scala.runtime.AbstractPartialFunction")
  lazy val SymbolClass                  = ctx.requiredClass("scala.Symbol")
  lazy val ClassClass                   = ctx.requiredClass("java.lang.Class")
  lazy val DynamicClass                 = ctx.requiredClass("scala.Dynamic")
  lazy val OptionClass                  = ctx.requiredClass("scala.Option")
  lazy val BoxedNumberClass             = ctx.requiredClass("java.lang.Number")
  lazy val ThrowableClass               = ctx.requiredClass("java.lang.Throwable")
  lazy val ClassCastExceptionClass      = ctx.requiredClass("java.lang.ClassCastException")
  lazy val JavaSerializableClass        = ctx.requiredClass("java.lang.Serializable")
  lazy val ComparableClass              = ctx.requiredClass("java.lang.Comparable")
  lazy val ProductClass                 = ctx.requiredClass("scala.Product")
  lazy val LanguageModuleClass          = ctx.requiredModule("dotty.language").moduleClass.asClass

  // Annotation base classes
  lazy val AnnotationClass              = ctx.requiredClass("scala.annotation.Annotation")
  lazy val ClassfileAnnotationClass     = ctx.requiredClass("scala.annotation.ClassfileAnnotation")
  lazy val StaticAnnotationClass        = ctx.requiredClass("scala.annotation.StaticAnnotation")

  // Annotation classes
  lazy val AliasAnnot = ctx.requiredClass("dotty.annotation.internal.Alias")
  lazy val ChildAnnot = ctx.requiredClass("dotty.annotation.internal.Child")
  lazy val InvariantBetweenClass = ctx.requiredClass("dotty.annotation.internal.InvariantBetween")
  lazy val CovariantBetweenClass = ctx.requiredClass("dotty.annotation.internal.CovariantBetween")
  lazy val ContravariantBetweenClass = ctx.requiredClass("dotty.annotation.internal.ContravariantBetween")
  lazy val ScalaSignatureAnnot = ctx.requiredClass("scala.reflect.ScalaSignature")
  lazy val ScalaLongSignatureAnnot = ctx.requiredClass("scala.reflect.ScalaLongSignature")
  lazy val DeprecatedAnnot = ctx.requiredClass("scala.deprecated")
  lazy val AnnotationDefaultAnnot = ctx.requiredClass("dotty.annotation.internal.AnnotationDefault")
  lazy val ThrowsAnnot = ctx.requiredClass("scala.throws")
  lazy val UncheckedAnnot = ctx.requiredClass("scala.unchecked")
  lazy val VolatileAnnot = ctx.requiredClass("scala.volatile")

  // convenient one-parameter method types
  def methOfAny(tp: Type) = MethodType(List(AnyType), tp)
  def methOfAnyVal(tp: Type) = MethodType(List(AnyValType), tp)
  def methOfAnyRef(tp: Type) = MethodType(List(ObjectType), tp)

  // Derived types
  def AnyType: Type = AnyClass.typeRef
  def AnyValType: Type = AnyValClass.typeRef
  def ObjectType: Type = ObjectClass.typeRef
  def AnyRefType: Type = AnyRefAlias.typeRef
  def NotNullType: Type = NotNullClass.typeRef
  def NothingType: Type = NothingClass.typeRef
  def NullType: Type = NullClass.typeRef
  def SeqType: Type = SeqClass.typeRef
  def ObjectArrayType = ArrayType(ObjectType)

  def UnitType: Type = UnitClass.typeRef
  def BooleanType: Type = BooleanClass.typeRef
  def ByteType: Type = ByteClass.typeRef
  def ShortType: Type = ShortClass.typeRef
  def CharType: Type = CharClass.typeRef
  def IntType: Type = IntClass.typeRef
  def LongType: Type = LongClass.typeRef
  def FloatType: Type = FloatClass.typeRef
  def DoubleType: Type = DoubleClass.typeRef
  def PairType: Type = PairClass.typeRef
  def StringType: Type = StringClass.typeRef
  def RepeatedParamType = RepeatedParamClass.typeRef
  def JavaRepeatedParamType = JavaRepeatedParamClass.typeRef
  def ThrowableType = ThrowableClass.typeRef
  def OptionType = OptionClass.typeRef
  def VolatileAnnotType = VolatileAnnot.typeRef

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
    def apply(args: List[Type], resultType: Type) =
      FunctionClass(args.length).typeRef.appliedTo(args ::: resultType :: Nil)
    def unapply(ft: Type)/*: Option[(List[Type], Type)]*/ = {
      // -language:keepUnions difference: unapply needs result type because inferred type
      // is Some[(List[Type], Type)] | None, which is not a legal unapply type.
      val tsym = ft.typeSymbol
      lazy val targs = ft.argInfos
      if ((FunctionClasses contains tsym) &&
          (targs.length - 1 <= MaxFunctionArity) &&
          (FunctionClass(targs.length - 1) == tsym)) Some(targs.init, targs.last)
      else None
    }
  }

  object ArrayType {
    def apply(elem: Type) =
      ArrayClass.typeRef.appliedTo(elem :: Nil)
    def unapply(tp: Type) = tp.dealias match {
      case at: RefinedType if (at isRef ArrayClass) && at.argInfos.length == 1 => Some(at.argInfos.head)
      case _ => None
    }
  }

  object MultiArrayType {
    def apply(elem: Type, ndims: Int): Type =
      if (ndims == 0) elem else ArrayType(apply(elem, ndims - 1))
    def unapply(tp: Type): Option[(Type, Int)] = tp match {
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

  lazy val FunctionClass = mkArityArray("Function", MaxFunctionArity, 0)
  lazy val TupleClass = mkArityArray("Tuple", MaxTupleArity, 2)
  lazy val ProductNClass = mkArityArray("Product", MaxTupleArity, 2)

  lazy val FunctionClasses: Set[Symbol] = FunctionClass.toSet
  lazy val TupleClasses: Set[Symbol] = TupleClass.toSet
  lazy val ProductClasses: Set[Symbol] = ProductNClass.toSet

  lazy val RepeatedParamClasses: Set[Symbol] = Set(RepeatedParamClass, JavaRepeatedParamClass)

  /** `Modules whose members are in the default namespace and their module classes */
  lazy val UnqualifiedOwners = RootImports.toSet ++ RootImports.map(_.moduleClass)

  lazy val PhantomClasses = Set[Symbol](AnyClass, AnyValClass, NullClass, NothingClass)

  lazy val asInstanceOfMethods = Set[Symbol](Any_asInstanceOf, Object_asInstanceOf)
  lazy val isInstanceOfMethods = Set[Symbol](Any_isInstanceOf, Object_isInstanceOf)

  lazy val RootImports = List[Symbol](JavaLangPackageVal, ScalaPackageVal, ScalaPredefModule, DottyPredefModule)

  def isTupleType(tp: Type) = {
    val arity = tp.dealias.argInfos.length
    arity <= MaxTupleArity && (tp isRef TupleClass(arity))
  }

  def isProductSubType(tp: Type) =
    (tp derivesFrom ProductClass) && tp.baseClasses.exists(ProductClasses contains _)

  def isFunctionType(tp: Type) = {
    val arity = functionArity(tp)
    0 <= arity && arity <= MaxFunctionArity && (tp isRef FunctionClass(arity))
  }

  def functionArity(tp: Type) = tp.dealias.argInfos.length - 1

  // ----- Higher kinds machinery ------------------------------------------

  private var _hkTraits: Set[Symbol] = Set()

  /** The set of HigherKindedXYZ traits encountered so far */
  def hkTraits: Set[Symbol] = _hkTraits

  private var hkTraitOfArity = mutable.Map[List[Int], ClassSymbol]()

  /** The HigherKinded trait corresponding to symbols `boundSyms` (which are assumed
   *  to be the type parameters of a higher-kided type). This is a class symbol that
   *  would be generated by the following schema.
   *
   *      class HigherKindedXYZ { type v_n _$hk$0; ...; type v_n _$Hk$n }
   *
   *  Here:
   *
   *   - XYZ is a string with one letter for each variant of a bound symbols,
   *     using `P` (positive variance), `N` (negative variance), `I` (invariant).
   *   - v_i are the variances of the bound symbols (i.e. +, -, or empty).
   *   - _$hk$i are higher-kinded parameter names, which are specially treated in type application.
   */
  def hkTrait(vcs: List[Int]) = {

    def varianceFlags(v: Int) = v match {
      case -1 => Contravariant
      case  0 => Covariant
      case  1 => EmptyFlags
    }

    val completer = new LazyType  {
      def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
        val cls = denot.asClass.classSymbol
        val paramDecls = newScope
        for (i <- 0 until vcs.length)
          newTypeParam(cls, tpnme.higherKindedParamName(i), EmptyFlags, paramDecls)
        denot.info = ClassInfo(ScalaPackageClass.thisType, cls, List(ObjectClass.typeRef), paramDecls)
      }
    }

    val traitName = tpnme.higherKindedTraitName(vcs)

    def createTrait = {
      val cls = newClassSymbol(
        ScalaPackageClass,
        traitName,
        Trait | Interface | Synthetic,
        completer)
      _hkTraits += cls
      cls
    }

    hkTraitOfArity.getOrElseUpdate(vcs, createTrait)
  }

  // ----- primitive value class machinery ------------------------------------------

  lazy val ScalaValueClasses: collection.Set[Symbol] = Set(
    UnitClass,
    BooleanClass,
    ByteClass,
    ShortClass,
    CharClass,
    IntClass,
    LongClass,
    FloatClass,
    DoubleClass)

  lazy val ScalaBoxedClasses = ScalaValueClasses map boxedClass

  private[this] val _boxedClass = mutable.Map[Symbol, Symbol]()
  private[this] val _unboxedClass = mutable.Map[Symbol, Symbol]()

  private[this] val _javaTypeToValueClass = mutable.Map[Class[_], Symbol]()
  private[this] val _valueClassToJavaType = mutable.Map[Symbol, Class[_]]()
  private[this] val _valueClassEnc = mutable.Map[Symbol, Int]()

  val boxedClass: collection.Map[Symbol, Symbol] = _boxedClass
  val unboxedClass: collection.Map[Symbol, Symbol] = _boxedClass
  val javaTypeToValueClass: collection.Map[Class[_], Symbol] = _javaTypeToValueClass
  val valueClassToJavaType: collection.Map[Symbol, Class[_]] = _valueClassToJavaType
  val valueClassEnc: collection.Map[Symbol, Int] = _valueClassEnc

  private def valueClassSymbol(name: String, boxed: ClassSymbol, jtype: Class[_], enc: Int): ClassSymbol = {
    val vcls = ctx.requiredClass(name)
    _unboxedClass(boxed) = vcls
    _boxedClass(vcls) = boxed
    _javaTypeToValueClass(jtype) = vcls
    _valueClassToJavaType(vcls) = jtype
    _valueClassEnc(vcls) = enc
    vcls
  }

  def wrapArrayMethodName(elemtp: Type): TermName = {
    val cls = elemtp.classSymbol
    if (cls.isPrimitiveValueClass) nme.wrapXArray(cls.name)
    else if (cls.derivesFrom(ObjectClass) && !cls.isPhantomClass) nme.wrapRefArray
    else nme.genericWrapArray
  }

  val ByteEnc = 2
  val ShortEnc = ByteEnc * 3
  val CharEnc = 5
  val IntEnc = ShortEnc * CharEnc
  val LongEnc = IntEnc * 7
  val FloatEnc = LongEnc * 11
  val DoubleEnc = FloatEnc * 13
  val BooleanEnc = 17
  val UnitEnc = 19

  def isValueSubClass(cls1: Symbol, cls2: Symbol) =
    valueClassEnc(cls2) % valueClassEnc(cls1) == 0

  // ----- Initialization ---------------------------------------------------

  /** Lists core classes that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
  lazy val syntheticCoreClasses = List(
    AnyRefAlias,
    RepeatedParamClass,
    JavaRepeatedParamClass,
    ByNameParamClass2x,
    AnyClass,
    AnyValClass,
    NullClass,
    NothingClass,
    SingletonClass,
    EqualsPatternClass,
    EmptyPackageVal)

    /** Lists core methods that don't have underlying bytecode, but are synthesized on-the-fly in every reflection universe */
    lazy val syntheticCoreMethods = List(
      Any_==,
      Any_!=,
      Any_equals,
      Any_hashCode,
      Any_toString,
      Any_getClass,
      Any_isInstanceOf,
      Any_asInstanceOf,
      Any_##,
      Object_eq,
      Object_ne,
      Object_==,
      Object_!=,
      Object_##,
      Object_synchronized,
      Object_isInstanceOf,
      Object_asInstanceOf,
      String_+
    )

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
