package dotty.tools
package backend
package jvm


import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.Erasure
import scala.tools.asm.{Handle, Opcodes}
import dotty.tools.dotc.core.StdNames

/**
 * Core BTypes and some other definitions. The initialization of these definitions requies access
 * to symbols / types (global).
 *
 * The symbols used to initialize the ClassBTypes may change from one compiler run to the next. To
 * make sure the definitions are consistent with the symbols in the current run, the
 * `intializeCoreBTypes` method in BTypesFromSymbols creates a new instance of CoreBTypes in each
 * compiler run.
 *
 * The class BTypesFromSymbols does not directly reference CoreBTypes, but CoreBTypesProxy. The
 * reason is that having a `var bTypes: CoreBTypes` would not allow `import bTypes._`. Instead, the
 * proxy class holds a `CoreBTypes` in a variable field and forwards to this instance.
 *
 * The definitions in `CoreBTypes` need to be lazy vals to break an initialization cycle. When
 * creating a new instance to assign to the proxy, the `classBTypeFromSymbol` invoked in the
 * constructor will actucally go through the proxy. The lazy vals make sure the instance is assigned
 * in the proxy before the fields are initialized.
 *
 * Note: if we did not re-create the core BTypes on each compiler run, BType.classBTypeFromInternalNameMap
 * could not be a perRunCache anymore: the classes defeined here need to be in that map, they are
 * added when the ClassBTypes are created. The per run cache removes them, so they would be missing
 * in the second run.
 */
class CoreBTypes[BTFS <: BTypesFromSymbols[_ <: DottyBackendInterface]](val bTypes: BTFS) {
  import bTypes._
  import int.given
  import DottyBackendInterface._

  //import global._
  //import rootMirror.{requiredClass, getClassIfDefined}
  //import definitions._

  /**
   * Maps primitive types to their corresponding PrimitiveBType. The map is defined lexically above
   * the first use of `classBTypeFromSymbol` because that method looks at the map.
   */
  lazy val primitiveTypeMap: Map[Symbol, PrimitiveBType] = Map(
    defn.UnitClass    -> UNIT,
    defn.BooleanClass -> BOOL,
    defn.CharClass    -> CHAR,
    defn.ByteClass    -> BYTE,
    defn.ShortClass   -> SHORT,
    defn.IntClass     -> INT,
    defn.LongClass    -> LONG,
    defn.FloatClass   -> FLOAT,
    defn.DoubleClass  -> DOUBLE
  )

  private lazy val BOXED_UNIT    : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.Void])
  private lazy val BOXED_BOOLEAN : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.Boolean])
  private lazy val BOXED_BYTE    : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.Byte])
  private lazy val BOXED_SHORT   : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.Short])
  private lazy val BOXED_CHAR    : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.Character])
  private lazy val BOXED_INT     : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.Integer])
  private lazy val BOXED_LONG    : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.Long])
  private lazy val BOXED_FLOAT   : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.Float])
  private lazy val BOXED_DOUBLE  : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.Double])

  /**
   * Map from primitive types to their boxed class type. Useful when pushing class literals onto the
   * operand stack (ldc instruction taking a class literal), see genConstant.
   */
  lazy val boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType] = Map(
    UNIT   -> BOXED_UNIT,
    BOOL   -> BOXED_BOOLEAN,
    BYTE   -> BOXED_BYTE,
    SHORT  -> BOXED_SHORT,
    CHAR   -> BOXED_CHAR,
    INT    -> BOXED_INT,
    LONG   -> BOXED_LONG,
    FLOAT  -> BOXED_FLOAT,
    DOUBLE -> BOXED_DOUBLE
  )

  lazy val boxedClasses: Set[ClassBType] = boxedClassOfPrimitive.values.toSet

  /**
   * Maps the method symbol for a box method to the boxed type of the result. For example, the
   * method symbol for `Byte.box()` is mapped to the ClassBType `java/lang/Byte`.
   */
  lazy val boxResultType: Map[Symbol, ClassBType] = {
    val boxMethods = defn.ScalaValueClasses().map{x => // @darkdimius Are you sure this should be a def?
      (x, Erasure.Boxing.boxMethod(x.asClass))
    }.toMap
    for ((valueClassSym, boxMethodSym) <- boxMethods)
    yield boxMethodSym -> boxedClassOfPrimitive(primitiveTypeMap(valueClassSym))
  }

  /**
   * Maps the method symbol for an unbox method to the primitive type of the result.
   * For example, the method symbol for `Byte.unbox()`) is mapped to the PrimitiveBType BYTE. */
  lazy val unboxResultType: Map[Symbol, PrimitiveBType] = {
    val unboxMethods: Map[Symbol, Symbol] =
      defn.ScalaValueClasses().map(x => (x, Erasure.Boxing.unboxMethod(x.asClass))).toMap
    for ((valueClassSym, unboxMethodSym) <- unboxMethods)
    yield unboxMethodSym -> primitiveTypeMap(valueClassSym)
  }

  /*
   * srNothingRef and srNullRef exist at run-time only. They are the bytecode-level manifestation (in
   * method signatures only) of what shows up as NothingClass (scala.Nothing) resp. NullClass (scala.Null) in Scala ASTs.
   *
   * Therefore, when srNothingRef or srNullRef are to be emitted, a mapping is needed: the internal
   * names of NothingClass and NullClass can't be emitted as-is.
   * TODO @lry Once there's a 2.11.3 starr, use the commented argument list. The current starr crashes on the type literal `scala.runtime.Nothing$`
   */
  lazy val srNothingRef : ClassBType = classBTypeFromSymbol(requiredClass("scala.runtime.Nothing$")) // (requiredClass[scala.runtime.Nothing$])
  lazy val srNullRef    : ClassBType = classBTypeFromSymbol(requiredClass("scala.runtime.Null$"))    // (requiredClass[scala.runtime.Null$])

  lazy val ObjectRef   : ClassBType = classBTypeFromSymbol(defn.ObjectClass)
  lazy val StringRef                   : ClassBType = classBTypeFromSymbol(defn.StringClass)
  lazy val jlStringBuilderRef          : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.StringBuilder])
  lazy val jlStringBufferRef           : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.StringBuffer])
  lazy val jlCharSequenceRef           : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.CharSequence])
  lazy val jlClassRef                  : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.Class[_]])
  lazy val jlThrowableRef              : ClassBType = classBTypeFromSymbol(defn.ThrowableClass)
  lazy val jlCloneableRef              : ClassBType = classBTypeFromSymbol(defn.JavaCloneableClass)        // java/lang/Cloneable
  lazy val jioSerializableRef          : ClassBType = classBTypeFromSymbol(requiredClass[java.io.Serializable])     // java/io/Serializable
  lazy val jlClassCastExceptionRef     : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.ClassCastException])   // java/lang/ClassCastException
  lazy val jlIllegalArgExceptionRef    : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.IllegalArgumentException])
  lazy val jliSerializedLambdaRef      : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.SerializedLambda])
  
  lazy val srBoxesRunTimeRef: ClassBType = classBTypeFromSymbol(requiredClass[scala.runtime.BoxesRunTime])

  private lazy val jliCallSiteRef              : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite])
  private lazy val jliLambdaMetafactoryRef     : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.LambdaMetafactory])
  private lazy val jliMethodHandleRef          : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodHandle])
  private lazy val jliMethodHandlesLookupRef   : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodHandles.Lookup])
  private lazy val jliMethodTypeRef            : ClassBType = classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType])
  private lazy val jliStringConcatFactoryRef   : ClassBType = classBTypeFromSymbol(requiredClass("java.lang.invoke.StringConcatFactory")) // since JDK 9
  private lazy val srLambdaDeserialize         : ClassBType = classBTypeFromSymbol(requiredClass[scala.runtime.LambdaDeserialize]) 

  lazy val jliLambdaMetaFactoryMetafactoryHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "metafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, jliMethodTypeRef, jliMethodHandleRef, jliMethodTypeRef),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)

  lazy val jliLambdaMetaFactoryAltMetafactoryHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "altMetafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)
    
  lazy val jliLambdaDeserializeBootstrapHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    srLambdaDeserialize.internalName,
    "bootstrap",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(jliMethodHandleRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)

  lazy val jliStringConcatFactoryMakeConcatWithConstantsHandle = new Handle(
    Opcodes.H_INVOKESTATIC,
    jliStringConcatFactoryRef.internalName,
    "makeConcatWithConstants",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, StringRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)
  
  /**
   * Methods in scala.runtime.BoxesRuntime
   */
  lazy val asmBoxTo  : Map[BType, MethodNameAndType] = Map(
    BOOL   -> MethodNameAndType("boxToBoolean",   MethodBType(List(BOOL),   BOXED_BOOLEAN)),
    BYTE   -> MethodNameAndType("boxToByte",      MethodBType(List(BYTE),   BOXED_BYTE)),
    CHAR   -> MethodNameAndType("boxToCharacter", MethodBType(List(CHAR),   BOXED_CHAR)),
    SHORT  -> MethodNameAndType("boxToShort",     MethodBType(List(SHORT),  BOXED_SHORT)),
    INT    -> MethodNameAndType("boxToInteger",   MethodBType(List(INT),    BOXED_INT)),
    LONG   -> MethodNameAndType("boxToLong",      MethodBType(List(LONG),   BOXED_LONG)),
    FLOAT  -> MethodNameAndType("boxToFloat",     MethodBType(List(FLOAT),  BOXED_FLOAT)),
    DOUBLE -> MethodNameAndType("boxToDouble",    MethodBType(List(DOUBLE), BOXED_DOUBLE))
  )

  lazy val asmUnboxTo: Map[BType, MethodNameAndType] = Map(
    BOOL   -> MethodNameAndType("unboxToBoolean", MethodBType(List(ObjectRef), BOOL)),
    BYTE   -> MethodNameAndType("unboxToByte",    MethodBType(List(ObjectRef), BYTE)),
    CHAR   -> MethodNameAndType("unboxToChar",    MethodBType(List(ObjectRef), CHAR)),
    SHORT  -> MethodNameAndType("unboxToShort",   MethodBType(List(ObjectRef), SHORT)),
    INT    -> MethodNameAndType("unboxToInt",     MethodBType(List(ObjectRef), INT)),
    LONG   -> MethodNameAndType("unboxToLong",    MethodBType(List(ObjectRef), LONG)),
    FLOAT  -> MethodNameAndType("unboxToFloat",   MethodBType(List(ObjectRef), FLOAT)),
    DOUBLE -> MethodNameAndType("unboxToDouble",  MethodBType(List(ObjectRef), DOUBLE))
  )

  lazy val typeOfArrayOp: Map[Int, BType] = {
    import dotty.tools.backend.ScalaPrimitivesOps._
    Map(
        (List(ZARRAY_LENGTH, ZARRAY_GET, ZARRAY_SET) map (_ -> BOOL))   ++
        (List(BARRAY_LENGTH, BARRAY_GET, BARRAY_SET) map (_ -> BYTE))   ++
        (List(SARRAY_LENGTH, SARRAY_GET, SARRAY_SET) map (_ -> SHORT))  ++
        (List(CARRAY_LENGTH, CARRAY_GET, CARRAY_SET) map (_ -> CHAR))   ++
        (List(IARRAY_LENGTH, IARRAY_GET, IARRAY_SET) map (_ -> INT))    ++
        (List(LARRAY_LENGTH, LARRAY_GET, LARRAY_SET) map (_ -> LONG))   ++
        (List(FARRAY_LENGTH, FARRAY_GET, FARRAY_SET) map (_ -> FLOAT))  ++
        (List(DARRAY_LENGTH, DARRAY_GET, DARRAY_SET) map (_ -> DOUBLE)) ++
        (List(OARRAY_LENGTH, OARRAY_GET, OARRAY_SET) map (_ -> ObjectRef)) : _*
    )
  }
}

/**
 * This trait make some core BTypes availalbe that don't depend on a Global instance. Some core
 * BTypes are required to be accessible in the BTypes trait, which does not have access to Global.
 *
 * BTypes cannot refer to CoreBTypesProxy because some of its members depend on global, for example
 * the type Symbol in
 *   def primitiveTypeMap: Map[Symbol, PrimitiveBType]
 */
trait CoreBTypesProxyGlobalIndependent[BTS <: BTypes] {
  val bTypes: BTS
  import bTypes._

  def boxedClasses: Set[ClassBType]

  def srNothingRef : ClassBType
  def srNullRef    : ClassBType

  def ObjectRef          : ClassBType
  def jlCloneableRef     : ClassBType
  def jiSerializableRef  : ClassBType
}

/**
 * See comment in class [[CoreBTypes]].
 */
final class CoreBTypesProxy[BTFS <: BTypesFromSymbols[_ <: DottyBackendInterface]](val bTypes: BTFS) extends CoreBTypesProxyGlobalIndependent[BTFS] {
  import bTypes._

  private var _coreBTypes: CoreBTypes[bTypes.type] = _
  def setBTypes(coreBTypes: CoreBTypes[BTFS]): Unit = {
    _coreBTypes = coreBTypes.asInstanceOf[CoreBTypes[bTypes.type]]
  }

  def primitiveTypeMap: Map[Symbol, PrimitiveBType] = _coreBTypes.primitiveTypeMap

  def boxedClasses: Set[ClassBType] = _coreBTypes.boxedClasses

  def boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType] = _coreBTypes.boxedClassOfPrimitive

  def boxResultType: Map[Symbol, ClassBType] = _coreBTypes.boxResultType

  def unboxResultType: Map[Symbol, PrimitiveBType] = _coreBTypes.unboxResultType

  def srNothingRef : ClassBType = _coreBTypes.srNothingRef
  def srNullRef    : ClassBType = _coreBTypes.srNullRef

  def ObjectRef                   : ClassBType = _coreBTypes.ObjectRef
  def StringRef                   : ClassBType = _coreBTypes.StringRef
  def jlStringBuilderRef          : ClassBType = _coreBTypes.jlStringBuilderRef
  def jlStringBufferRef           : ClassBType = _coreBTypes.jlStringBufferRef
  def jlCharSequenceRef           : ClassBType = _coreBTypes.jlCharSequenceRef
  def jlClassRef                  : ClassBType = _coreBTypes.jlClassRef
  def jlThrowableRef              : ClassBType = _coreBTypes.jlThrowableRef
  def jlCloneableRef              : ClassBType = _coreBTypes.jlCloneableRef
  def jiSerializableRef           : ClassBType = _coreBTypes.jioSerializableRef
  def jlClassCastExceptionRef     : ClassBType = _coreBTypes.jlClassCastExceptionRef
  def jlIllegalArgExceptionRef    : ClassBType = _coreBTypes.jlIllegalArgExceptionRef
  def jliSerializedLambdaRef      : ClassBType = _coreBTypes.jliSerializedLambdaRef

  def srBoxesRuntimeRef: ClassBType = _coreBTypes.srBoxesRunTimeRef

  def jliLambdaMetaFactoryMetafactoryHandle    : Handle = _coreBTypes.jliLambdaMetaFactoryMetafactoryHandle
  def jliLambdaMetaFactoryAltMetafactoryHandle : Handle = _coreBTypes.jliLambdaMetaFactoryAltMetafactoryHandle
  def jliLambdaDeserializeBootstrapHandle      : Handle = _coreBTypes.jliLambdaDeserializeBootstrapHandle
  def jliStringConcatFactoryMakeConcatWithConstantsHandle: Handle = _coreBTypes.jliStringConcatFactoryMakeConcatWithConstantsHandle
  
  def asmBoxTo  : Map[BType, MethodNameAndType] = _coreBTypes.asmBoxTo
  def asmUnboxTo: Map[BType, MethodNameAndType] = _coreBTypes.asmUnboxTo

  def typeOfArrayOp: Map[Int, BType] = _coreBTypes.typeOfArrayOp
}
