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

class KnownBTypes(loader: BTypeLoader)(using @constructorOnly initctx: Context) {
  val ObjectRef: ClassBType = loader.classBTypeFromSymbol(defn.ObjectClass)
  val StringRef: ClassBType = loader.classBTypeFromSymbol(defn.StringClass)

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
