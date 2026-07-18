package dotty.tools.backend.jvm

import dotty.tools.dotc.core.Symbols.*

import scala.tools.asm.{Handle, Opcodes}
import dotty.tools.dotc.core.Contexts.Context

import scala.annotation.constructorOnly

class KnownBTypes(loader: BTypeLoader)(using @constructorOnly initctx: Context) {
  val ObjectRef: ClassBType = loader.classBTypeFromSymbol(defn.ObjectClass)
  val StringRef: ClassBType = loader.classBTypeFromSymbol(defn.StringClass)

  val jlThrowableRef: ClassBType = loader.classBTypeFromSymbol(defn.ThrowableClass)

  private val jliLambdaMetafactoryInternalName: String = "java/lang/invoke/LambdaMetafactory"
  private val jliStringConcatFactoryInternalName: String = "java/lang/invoke/StringConcatFactory"
  private val jliMethodHandlesLookupRef: ClassBType = loader.classBTypeFromSymbol(defn.MethodHandlesLookupClass)
  private val jliMethodTypeRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType])
  private val jliMethodHandleRef: ClassBType = loader.classBTypeFromSymbol(defn.MethodHandleClass)
  private val jliCallSiteRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite])
  private val srLambdaDeserializeInternalName: String = "scala/runtime/LambdaDeserialize"
  val jliLambdaDeserializeBootstrapHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    srLambdaDeserializeInternalName,
    "bootstrap",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(jliMethodHandleRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false
  )
  val jliLambdaMetaFactoryMetafactoryHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryInternalName,
    "metafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, jliMethodTypeRef, jliMethodHandleRef, jliMethodTypeRef),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false
  )
  val jliLambdaMetaFactoryAltMetafactoryHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryInternalName,
    "altMetafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false
  )
  val jliStringConcatFactoryMakeConcatWithConstantsHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    jliStringConcatFactoryInternalName,
    "makeConcatWithConstants",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, StringRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false
  )

  /**
   * Map from primitive types to their boxed class type.
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
}
