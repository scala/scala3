package dotty.tools
package backend
package jvm


import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.transform.Erasure
import scala.tools.asm.{Handle, Opcodes}
import dotty.tools.dotc.core.StdNames
import BTypes.InternalName
import PostProcessorFrontendAccess.Lazy

abstract class CoreBTypes {
  val bTypes: BTypes
  import bTypes.*

   def primitiveTypeMap: Map[Symbol, PrimitiveBType]

   def boxedClasses: Set[ClassBType]

   def boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType]

   def boxResultType: Map[Symbol, ClassBType]

   def unboxResultType: Map[Symbol, PrimitiveBType]

   def srNothingRef : ClassBType
   def srNullRef    : ClassBType

   def ObjectRef                   : ClassBType
   def StringRef                   : ClassBType
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

  import bTypes.*
  import DottyBackendInterface.*
  import dotty.tools.dotc.core.Contexts.Context
  import frontendAccess.perRunLazy
  /**
   * Maps primitive types to their corresponding PrimitiveBType. The map is defined lexically above
   * the first use of `classBTypeFromSymbol` because that method looks at the map.
   */
  override def primitiveTypeMap: Map[Symbol, bTypes.PrimitiveBType] = _primitiveTypeMap.get
  private lazy val _primitiveTypeMap: Lazy[Map[Symbol, PrimitiveBType]] = perRunLazy:
    Map(
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
  override def boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType] = _boxedClassOfPrimitive.get
  private lazy val _boxedClassOfPrimitive: Lazy[Map[PrimitiveBType, ClassBType]] = perRunLazy(Map(
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
  override def boxResultType: Map[Symbol, ClassBType] = _boxResultType.get
  private lazy val _boxResultType: Lazy[Map[Symbol, ClassBType]] = perRunLazy{
    val boxMethods = defn.ScalaValueClasses().map{x => // @darkdimius Are you sure this should be a def?
      (x, Erasure.Boxing.boxMethod(x.asClass))
    }.toMap
    for ((valueClassSym, boxMethodSym) <- boxMethods)
    yield boxMethodSym -> boxedClassOfPrimitive(primitiveTypeMap(valueClassSym))
  }

  /**
   * Maps the method symbol for an unbox method to the primitive type of the result.
   * For example, the method symbol for `Byte.unbox()`) is mapped to the PrimitiveBType BYTE. */
  override def unboxResultType: Map[Symbol, PrimitiveBType] = _unboxResultType.get
  private lazy val _unboxResultType = perRunLazy[Map[Symbol, PrimitiveBType]]{
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
  override def srNothingRef: ClassBType = _srNothingRef.get
  private lazy val _srNothingRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass("scala.runtime.Nothing$")))

  override def srNullRef: ClassBType = _srNullRef.get
  private lazy val _srNullRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass("scala.runtime.Null$")))

  override def ObjectRef: ClassBType = _ObjectRef.get
  private lazy val _ObjectRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(defn.ObjectClass))

  override def StringRef: ClassBType = _StringRef.get
  private lazy val _StringRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(defn.StringClass))

  override def jlClassRef: ClassBType = _jlClassRef.get
  private lazy val _jlClassRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.Class[?]]))

  override def jlThrowableRef: ClassBType = _jlThrowableRef.get
  private lazy val _jlThrowableRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(defn.ThrowableClass))

  override def jlCloneableRef: ClassBType = _jlCloneableRef.get
  private lazy val _jlCloneableRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(defn.JavaCloneableClass))

  override def jiSerializableRef: ClassBType = _jiSerializableRef.get
  private lazy val _jiSerializableRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass[java.io.Serializable]))

  override def jlClassCastExceptionRef: ClassBType = _jlClassCastExceptionRef.get
  private lazy val _jlClassCastExceptionRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.ClassCastException]))

  override def jlIllegalArgExceptionRef: ClassBType = _jlIllegalArgExceptionRef.get
  private lazy val _jlIllegalArgExceptionRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.IllegalArgumentException]))

  override def jliSerializedLambdaRef: ClassBType = _jliSerializedLambdaRef.get
  private lazy val _jliSerializedLambdaRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.SerializedLambda]))

  override def srBoxesRuntimeRef: ClassBType = _srBoxesRuntimeRef.get
  private lazy val _srBoxesRuntimeRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass[scala.runtime.BoxesRunTime]))

  private def jliCallSiteRef: ClassBType = _jliCallSiteRef.get
  private lazy val _jliCallSiteRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite]))

  private def jliLambdaMetafactoryRef: ClassBType = _jliLambdaMetafactoryRef.get
  private lazy val _jliLambdaMetafactoryRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.LambdaMetafactory]))

  private def jliMethodHandleRef: ClassBType = _jliMethodHandleRef.get
  private lazy val _jliMethodHandleRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(defn.MethodHandleClass))

  private def jliMethodHandlesLookupRef: ClassBType = _jliMethodHandlesLookupRef.get
  private lazy val _jliMethodHandlesLookupRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(defn.MethodHandlesLookupClass))

  private def jliMethodTypeRef: ClassBType = _jliMethodTypeRef.get
  private lazy val _jliMethodTypeRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType]))

  // since JDK 9
  private def jliStringConcatFactoryRef: ClassBType = _jliStringConcatFactoryRef.get
  private lazy val _jliStringConcatFactoryRef: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass("java.lang.invoke.StringConcatFactory")))

  private def srLambdaDeserialize: ClassBType = _srLambdaDeserialize.get
  private lazy val _srLambdaDeserialize: Lazy[ClassBType] = perRunLazy(classBTypeFromSymbol(requiredClass[scala.runtime.LambdaDeserialize]))


  override def jliLambdaMetaFactoryMetafactoryHandle = _jliLambdaMetaFactoryMetafactoryHandle.get
  private lazy val _jliLambdaMetaFactoryMetafactoryHandle: Lazy[Handle] = perRunLazy{new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "metafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, jliMethodTypeRef, jliMethodHandleRef, jliMethodTypeRef),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  override def jliLambdaMetaFactoryAltMetafactoryHandle = _jliLambdaMetaFactoryAltMetafactoryHandle.get
  private lazy val _jliLambdaMetaFactoryAltMetafactoryHandle: Lazy[Handle] = perRunLazy{ new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "altMetafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  override def jliLambdaDeserializeBootstrapHandle: Handle = _jliLambdaDeserializeBootstrapHandle.get
  private lazy val _jliLambdaDeserializeBootstrapHandle: Lazy[Handle] = perRunLazy{ new Handle(
    Opcodes.H_INVOKESTATIC,
    srLambdaDeserialize.internalName,
    "bootstrap",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(jliMethodHandleRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  override def jliStringConcatFactoryMakeConcatWithConstantsHandle = _jliStringConcatFactoryMakeConcatWithConstantsHandle.get
  private lazy val _jliStringConcatFactoryMakeConcatWithConstantsHandle: Lazy[Handle] = perRunLazy{ new Handle(
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
   * No need to wrap in Lazy to synchronize access, symbols won't change
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
    import dotty.tools.backend.ScalaPrimitivesOps.*
    Map(
        (List(ZARRAY_LENGTH, ZARRAY_GET, ZARRAY_SET) map (_ -> BOOL))   ++
        (List(BARRAY_LENGTH, BARRAY_GET, BARRAY_SET) map (_ -> BYTE))   ++
        (List(SARRAY_LENGTH, SARRAY_GET, SARRAY_SET) map (_ -> SHORT))  ++
        (List(CARRAY_LENGTH, CARRAY_GET, CARRAY_SET) map (_ -> CHAR))   ++
        (List(IARRAY_LENGTH, IARRAY_GET, IARRAY_SET) map (_ -> INT))    ++
        (List(LARRAY_LENGTH, LARRAY_GET, LARRAY_SET) map (_ -> LONG))   ++
        (List(FARRAY_LENGTH, FARRAY_GET, FARRAY_SET) map (_ -> FLOAT))  ++
        (List(DARRAY_LENGTH, DARRAY_GET, DARRAY_SET) map (_ -> DOUBLE)) ++
        (List(OARRAY_LENGTH, OARRAY_GET, OARRAY_SET) map (_ -> ObjectRef)) *
    )
  }
}
