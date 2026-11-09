package dotty.tools.backend.jvm

import dotty.tools.initialize
import dotty.tools.dotc.core.Symbols.*
import org.objectweb.asm.{Handle, Opcodes}
import dotty.tools.dotc.core.Contexts.Context

import scala.annotation.constructorOnly
import scala.collection.mutable

// To improve the latency of compiling simple programs, this class's properties are lazy-loaded,
// except ObjectRef which is always used
class KnownBTypes(loader: BTypeLoader)(using @constructorOnly initctx: Context) {
  val ObjectRef: ClassBType = loader.classBTypeFromSymbol(defn.ObjectClass)

  private var _stringRef: ClassBType | Null = null
  def StringRef(using Context): ClassBType =
    initialize(_stringRef, _stringRef = _, loader.classBTypeFromSymbol(defn.StringClass))

  private var _jlThrowableRef: ClassBType | Null = null
  def jlThrowableRef(using Context): ClassBType =
    initialize(_jlThrowableRef, _jlThrowableRef = _, loader.classBTypeFromSymbol(defn.ThrowableClass))

  private val srLambdaDeserializeInternalName: String = "scala/runtime/LambdaDeserialize"
  private val jliLambdaMetafactoryInternalName: String = "java/lang/invoke/LambdaMetafactory"
  private val jliStringConcatFactoryInternalName: String = "java/lang/invoke/StringConcatFactory"

  private var _jliMethodHandlesLookupRef: ClassBType | Null = null
  private def jliMethodHandlesLookupRef(using Context): ClassBType =
    initialize(_jliMethodHandlesLookupRef, _jliMethodHandlesLookupRef = _, loader.classBTypeFromSymbol(defn.MethodHandlesLookupClass))

  private var _jliMethodTypeRef: ClassBType | Null = null
  private def jliMethodTypeRef(using Context): ClassBType =
    initialize(_jliMethodTypeRef, _jliMethodTypeRef = _, loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType]))

  private var _jliMethodHandleRef: ClassBType | Null = null
  private def jliMethodHandleRef(using Context): ClassBType =
    initialize(_jliMethodHandleRef, _jliMethodHandleRef = _, loader.classBTypeFromSymbol(defn.MethodHandleClass))

  private var _jliCallSiteRef: ClassBType | Null = null
  private def jliCallSiteRef(using Context): ClassBType =
    initialize(_jliCallSiteRef, _jliCallSiteRef = _, loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite]))

  private var _jliLambdaDeserializeBootstrapHandle: Handle | Null = null
  def jliLambdaDeserializeBootstrapHandle(using Context): Handle =
    initialize(_jliLambdaDeserializeBootstrapHandle, _jliLambdaDeserializeBootstrapHandle = _, new Handle(
      Opcodes.H_INVOKESTATIC,
      srLambdaDeserializeInternalName,
      "bootstrap",
      MethodBType(
        List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(jliMethodHandleRef)),
        jliCallSiteRef
      ).descriptor,
      /* itf = */ false
    ))

  private var _jliLambdaMetaFactoryMetafactoryHandle: Handle | Null = null
  def jliLambdaMetaFactoryMetafactoryHandle(using Context): Handle =
    initialize(_jliLambdaMetaFactoryMetafactoryHandle, _jliLambdaMetaFactoryMetafactoryHandle = _, new Handle(
      Opcodes.H_INVOKESTATIC,
      jliLambdaMetafactoryInternalName,
      "metafactory",
      MethodBType(
        List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, jliMethodTypeRef, jliMethodHandleRef, jliMethodTypeRef),
        jliCallSiteRef
      ).descriptor,
      /* itf = */ false
    ))

  private var _jliLambdaMetaFactoryAltMetafactoryHandle: Handle | Null = null
  def jliLambdaMetaFactoryAltMetafactoryHandle(using Context): Handle =
    initialize(_jliLambdaMetaFactoryAltMetafactoryHandle, _jliLambdaMetaFactoryAltMetafactoryHandle = _, new Handle(
      Opcodes.H_INVOKESTATIC,
      jliLambdaMetafactoryInternalName,
      "altMetafactory",
      MethodBType(
        List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(ObjectRef)),
        jliCallSiteRef
      ).descriptor,
      /* itf = */ false
    ))

  private var _jliStringConcatFactoryMakeConcatWithConstantsHandle: Handle | Null = null
  def jliStringConcatFactoryMakeConcatWithConstantsHandle(using Context): Handle =
    initialize(_jliStringConcatFactoryMakeConcatWithConstantsHandle, _jliStringConcatFactoryMakeConcatWithConstantsHandle = _, new Handle(
      Opcodes.H_INVOKESTATIC,
      jliStringConcatFactoryInternalName,
      "makeConcatWithConstants",
      MethodBType(
        List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, StringRef, ArrayBType(ObjectRef)),
        jliCallSiteRef
      ).descriptor,
      /* itf = */ false
    ))

  private val _boxedClassesOfPrimitive = mutable.Map.empty[PrimitiveBType, ClassBType]
  /**
   * Map from primitive types to their boxed class type.
   */
  def boxedClassOfPrimitive(bType: PrimitiveBType)(using Context): ClassBType =
    _boxedClassesOfPrimitive.getOrElseUpdate(bType, loader.classBTypeFromSymbol(requiredClass(bType.boxedClass.getName)))
}
