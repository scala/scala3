package dotty.tools
package backend
package jvm


import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.Erasure
import scala.tools.asm.{Handle, Opcodes}
import dotty.tools.dotc.core.StdNames
import BTypes.InternalName

abstract class CoreBTypes {
  val bTypes: BTypes
  import bTypes._

   def primitiveTypeMap: Map[Symbol, PrimitiveBType]

   def boxedClasses: Set[ClassBType]

   def boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType]

   def boxResultType: Map[Symbol, ClassBType]

   def unboxResultType: Map[Symbol, PrimitiveBType]

   def srNothingRef : ClassBType
   def srNullRef    : ClassBType

   def ObjectRef                   : ClassBType
   def StringRef                   : ClassBType
   def jlStringBuilderRef          : ClassBType
   def jlStringBufferRef           : ClassBType
   def jlCharSequenceRef           : ClassBType
   def jlClassRef                  : ClassBType
   def jlThrowableRef              : ClassBType
   def jlCloneableRef              : ClassBType
   def jiSerializableRef           : ClassBType
   def jlClassCastExceptionRef     : ClassBType
   def jlIllegalArgExceptionRef    : ClassBType
   def jliSerializedLambdaRef      : ClassBType

   def srBoxesRuntimeRef: ClassBType

   def jliLambdaMetaFactoryMetafactoryHandle    : Handle
   def jliLambdaMetaFactoryAltMetafactoryHandle : Handle
   def jliLambdaDeserializeBootstrapHandle      : Handle
   def jliStringConcatFactoryMakeConcatWithConstantsHandle: Handle

   def asmBoxTo  : Map[BType, MethodNameAndType]
   def asmUnboxTo: Map[BType, MethodNameAndType]

   def typeOfArrayOp: Map[Int, BType]
}

abstract class CoreBTypesFromSymbols[I <: DottyBackendInterface] extends CoreBTypes {
  val bTypes: BTypesFromSymbols[I]

  import bTypes._
  import int.given
  import DottyBackendInterface._
  import frontendAccess.frontendSynch
  import dotty.tools.dotc.core.Contexts.Context

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

  /**
   * Map from primitive types to their boxed class type. Useful when pushing class literals onto the
   * operand stack (ldc instruction taking a class literal), see genConstant.
   */
  lazy val boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType] = frontendSynch(Map(
      UNIT   -> classBTypeFromSymbol(requiredClass[java.lang.Void]),
      BOOL   -> classBTypeFromSymbol(requiredClass[java.lang.Boolean]),
      BYTE   -> classBTypeFromSymbol(requiredClass[java.lang.Byte]),
      SHORT  -> classBTypeFromSymbol(requiredClass[java.lang.Short]),
      CHAR   -> classBTypeFromSymbol(requiredClass[java.lang.Character]),
      INT    -> classBTypeFromSymbol(requiredClass[java.lang.Integer]),
      LONG   -> classBTypeFromSymbol(requiredClass[java.lang.Long]),
      FLOAT  -> classBTypeFromSymbol(requiredClass[java.lang.Float]),
      DOUBLE -> classBTypeFromSymbol(requiredClass[java.lang.Double])
    ))

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

  // Used to synchronize initialization of Context dependent ClassBTypes which can be accessed from multiple-threads
  // Unsychronized initialization might lead errors in either CodeGen or PostProcessor
  inline private def synchClassBTypeFromSymbol(inline sym: Symbol) = frontendSynch(classBTypeFromSymbol(sym))

  /*
   * srNothingRef and srNullRef exist at run-time only. They are the bytecode-level manifestation (in
   * method signatures only) of what shows up as NothingClass (scala.Nothing) resp. NullClass (scala.Null) in Scala ASTs.
   *
   * Therefore, when srNothingRef or srNullRef are to be emitted, a mapping is needed: the internal
   * names of NothingClass and NullClass can't be emitted as-is.
   * TODO @lry Once there's a 2.11.3 starr, use the commented argument list. The current starr crashes on the type literal `scala.runtime.Nothing$`
   */
  lazy val srNothingRef : ClassBType = synchClassBTypeFromSymbol(requiredClass("scala.runtime.Nothing$"))
  lazy val srNullRef    : ClassBType = synchClassBTypeFromSymbol(requiredClass("scala.runtime.Null$"))

  lazy val ObjectRef : ClassBType = synchClassBTypeFromSymbol(defn.ObjectClass)
  lazy val StringRef : ClassBType = synchClassBTypeFromSymbol(defn.StringClass)

  lazy val jlStringBuilderRef       : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.lang.StringBuilder])
  lazy val jlStringBufferRef        : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.lang.StringBuffer])
  lazy val jlCharSequenceRef        : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.lang.CharSequence])
  lazy val jlClassRef               : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.lang.Class[?]])
  lazy val jlThrowableRef           : ClassBType = synchClassBTypeFromSymbol(defn.ThrowableClass)
  lazy val jlCloneableRef           : ClassBType = synchClassBTypeFromSymbol(defn.JavaCloneableClass)
  lazy val jiSerializableRef        : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.io.Serializable])
  lazy val jlClassCastExceptionRef  : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.lang.ClassCastException])
  lazy val jlIllegalArgExceptionRef : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.lang.IllegalArgumentException])
  lazy val jliSerializedLambdaRef   : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.lang.invoke.SerializedLambda])

  lazy val srBoxesRuntimeRef: ClassBType = synchClassBTypeFromSymbol(requiredClass[scala.runtime.BoxesRunTime])

  private lazy val jliCallSiteRef            : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite])
  private lazy val jliLambdaMetafactoryRef   : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.lang.invoke.LambdaMetafactory])
  private lazy val jliMethodHandleRef        : ClassBType = synchClassBTypeFromSymbol(defn.MethodHandleClass)
  private lazy val jliMethodHandlesLookupRef : ClassBType = synchClassBTypeFromSymbol(defn.MethodHandlesLookupClass)
  private lazy val jliMethodTypeRef          : ClassBType = synchClassBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType])
  private lazy val jliStringConcatFactoryRef : ClassBType = synchClassBTypeFromSymbol(requiredClass("java.lang.invoke.StringConcatFactory")) // since JDK 9

  lazy val srLambdaDeserialize               : ClassBType = synchClassBTypeFromSymbol(requiredClass[scala.runtime.LambdaDeserialize])

  lazy val jliLambdaMetaFactoryMetafactoryHandle = frontendSynch{ new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "metafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, jliMethodTypeRef, jliMethodHandleRef, jliMethodTypeRef),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  lazy val jliLambdaMetaFactoryAltMetafactoryHandle = frontendSynch{ new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "altMetafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  lazy val jliLambdaDeserializeBootstrapHandle: Handle = frontendSynch{ new Handle(
    Opcodes.H_INVOKESTATIC,
    srLambdaDeserialize.internalName,
    "bootstrap",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(jliMethodHandleRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  lazy val jliStringConcatFactoryMakeConcatWithConstantsHandle = frontendSynch{ new Handle(
    Opcodes.H_INVOKESTATIC,
    jliStringConcatFactoryRef.internalName,
    "makeConcatWithConstants",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, StringRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  /**
   * Methods in scala.runtime.BoxesRuntime
   */
  lazy val asmBoxTo  : Map[BType, MethodNameAndType] = Map(
    BOOL   -> MethodNameAndType("boxToBoolean",   MethodBType(List(BOOL),   boxedClassOfPrimitive(BOOL))),
    BYTE   -> MethodNameAndType("boxToByte",      MethodBType(List(BYTE),   boxedClassOfPrimitive(BYTE))),
    CHAR   -> MethodNameAndType("boxToCharacter", MethodBType(List(CHAR),   boxedClassOfPrimitive(CHAR))),
    SHORT  -> MethodNameAndType("boxToShort",     MethodBType(List(SHORT),  boxedClassOfPrimitive(SHORT))),
    INT    -> MethodNameAndType("boxToInteger",   MethodBType(List(INT),    boxedClassOfPrimitive(INT))),
    LONG   -> MethodNameAndType("boxToLong",      MethodBType(List(LONG),   boxedClassOfPrimitive(LONG))),
    FLOAT  -> MethodNameAndType("boxToFloat",     MethodBType(List(FLOAT),  boxedClassOfPrimitive(FLOAT))),
    DOUBLE -> MethodNameAndType("boxToDouble",    MethodBType(List(DOUBLE), boxedClassOfPrimitive(DOUBLE)))
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
