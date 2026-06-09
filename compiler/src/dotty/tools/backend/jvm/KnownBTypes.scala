package dotty.tools.backend.jvm

import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.transform.Erasure

import scala.tools.asm.{Handle, Opcodes}
import dotty.tools.dotc.core.Symbols
import BTypes.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.backend.ScalaPrimitivesOps.*

import scala.annotation.constructorOnly

case class MethodNameAndType(name: String, methodType: MethodBType)

class KnownBTypes(loader: BTypeLoader)(using @constructorOnly initctx: Context) {
  val ObjectRef: ClassBType = loader.classBTypeFromSymbol(defn.ObjectClass)
  val StringRef: ClassBType = loader.classBTypeFromSymbol(defn.StringClass)
  val srNullRef: ClassBType = loader.classBTypeFromSymbol(defn.RuntimeNullClass)

  val jlThrowableRef: ClassBType = loader.classBTypeFromSymbol(defn.ThrowableClass)

  protected val jliLambdaMetafactoryRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.LambdaMetafactory])
  protected val jliStringConcatFactoryRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.StringConcatFactory])
  protected val jliMethodHandlesLookupRef: ClassBType = loader.classBTypeFromSymbol(defn.MethodHandlesLookupClass)
  protected val jliMethodTypeRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType])
  protected val jliMethodHandleRef: ClassBType = loader.classBTypeFromSymbol(defn.MethodHandleClass)
  protected val jliCallSiteRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite])
  protected val srLambdaDeserialize: ClassBType = loader.classBTypeFromSymbol(requiredClass[scala.runtime.LambdaDeserialize])
  val jliLambdaDeserializeBootstrapHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    srLambdaDeserialize.internalName,
    "bootstrap",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(jliMethodHandleRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false
  )
  val jliLambdaMetaFactoryMetafactoryHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "metafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, jliMethodTypeRef, jliMethodHandleRef, jliMethodTypeRef),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false
  )
  val jliLambdaMetaFactoryAltMetafactoryHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "altMetafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false
  )
  val jliStringConcatFactoryMakeConcatWithConstantsHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    jliStringConcatFactoryRef.internalName,
    "makeConcatWithConstants",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, StringRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false
  )

  /**
   * Map from primitive types to their boxed class type. Useful when pushing class literals onto the
   * operand stack (ldc instruction taking a class literal), see genConstant.
   */
  val boxedClassOfPrimitive: Map[BType, ClassBType] = Map(
    UNIT   -> loader.classBTypeFromSymbol(requiredClass[java.lang.Void]),
    BOOL   -> loader.classBTypeFromSymbol(requiredClass[java.lang.Boolean]),
    BYTE   -> loader.classBTypeFromSymbol(requiredClass[java.lang.Byte]),
    SHORT  -> loader.classBTypeFromSymbol(requiredClass[java.lang.Short]),
    CHAR   -> loader.classBTypeFromSymbol(requiredClass[java.lang.Character]),
    INT    -> loader.classBTypeFromSymbol(requiredClass[java.lang.Integer]),
    LONG   -> loader.classBTypeFromSymbol(requiredClass[java.lang.Long]),
    FLOAT  -> loader.classBTypeFromSymbol(requiredClass[java.lang.Float]),
    DOUBLE -> loader.classBTypeFromSymbol(requiredClass[java.lang.Double])
  )

  val asmBoxTo: Map[BType, MethodNameAndType] = Map(
    BOOL   -> MethodNameAndType("boxToBoolean",   MethodBType(List(BOOL),   boxedClassOfPrimitive(BOOL))),
    BYTE   -> MethodNameAndType("boxToByte",      MethodBType(List(BYTE),   boxedClassOfPrimitive(BYTE))),
    CHAR   -> MethodNameAndType("boxToCharacter", MethodBType(List(CHAR),   boxedClassOfPrimitive(CHAR))),
    SHORT  -> MethodNameAndType("boxToShort",     MethodBType(List(SHORT),  boxedClassOfPrimitive(SHORT))),
    INT    -> MethodNameAndType("boxToInteger",   MethodBType(List(INT),    boxedClassOfPrimitive(INT))),
    LONG   -> MethodNameAndType("boxToLong",      MethodBType(List(LONG),   boxedClassOfPrimitive(LONG))),
    FLOAT  -> MethodNameAndType("boxToFloat",     MethodBType(List(FLOAT),  boxedClassOfPrimitive(FLOAT))),
    DOUBLE -> MethodNameAndType("boxToDouble",    MethodBType(List(DOUBLE), boxedClassOfPrimitive(DOUBLE)))
  )
  val asmUnboxTo: Map[BType, MethodNameAndType] = Map(
    BOOL   -> MethodNameAndType("unboxToBoolean", MethodBType(List(ObjectRef), BOOL)),
    BYTE   -> MethodNameAndType("unboxToByte",    MethodBType(List(ObjectRef), BYTE)),
    CHAR   -> MethodNameAndType("unboxToChar",    MethodBType(List(ObjectRef), CHAR)),
    SHORT  -> MethodNameAndType("unboxToShort",   MethodBType(List(ObjectRef), SHORT)),
    INT    -> MethodNameAndType("unboxToInt",     MethodBType(List(ObjectRef), INT)),
    LONG   -> MethodNameAndType("unboxToLong",    MethodBType(List(ObjectRef), LONG)),
    FLOAT  -> MethodNameAndType("unboxToFloat",   MethodBType(List(ObjectRef), FLOAT)),
    DOUBLE -> MethodNameAndType("unboxToDouble",  MethodBType(List(ObjectRef), DOUBLE))
  )
}
