package dotty.tools
package backend
package jvm

import java.util.concurrent.ConcurrentHashMap
import BTypes.InternalName
import BackendReporting.NoClassBTypeInfo
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.backend.jvm.PostProcessorFrontendAccess.Lazy

import scala.tools.asm.Handle

case class MethodNameAndType(name: String, methodType: MethodBType)

abstract class CoreBTypes(private val frontendAccess: PostProcessorFrontendAccess)(using ctx: Context) {
  def primitiveTypeMap: Map[Symbol, PrimitiveBType]

  def boxedClasses: Set[ClassBType]

  def boxedClassOfPrimitive: Map[PrimitiveBType, ClassBType]

  def boxResultType: Map[Symbol, ClassBType]

  def unboxResultType: Map[Symbol, PrimitiveBType]

  def srNothingRef : ClassBType
  def srNullRef    : ClassBType

  def ObjectRef                   : ClassBType
  def StringRef                   : ClassBType
  def PredefRef                   : ClassBType
  def jlClassRef                  : ClassBType
  def jlThrowableRef              : ClassBType
  def jlCloneableRef              : ClassBType
  def jiSerializableRef           : ClassBType
  def jlClassCastExceptionRef     : ClassBType
  def jlIllegalArgExceptionRef    : ClassBType
  def jliSerializedLambdaRef      : ClassBType
  def jliMethodHandleRef: ClassBType

  def srBoxesRuntimeRef            : ClassBType
  def srBoxedUnitRef               : ClassBType
  def srBoxesRuntimeBoxToMethods   : Map[BType, MethodNameAndType]
  def srBoxesRuntimeUnboxToMethods : Map[BType, MethodNameAndType]

  def javaBoxMethods   : Map[InternalName, MethodNameAndType]
  def javaUnboxMethods : Map[InternalName, MethodNameAndType]

  def predefAutoBoxMethods   : Map[String, MethodBType]
  def predefAutoUnboxMethods : Map[String, MethodBType]

  def srRefCreateMethods : Map[InternalName, MethodNameAndType]
  def srRefZeroMethods   : Map[InternalName, MethodNameAndType]

  def primitiveBoxConstructors : Map[InternalName, MethodNameAndType]
  def srRefConstructors        : Map[InternalName, MethodNameAndType]
  def tupleClassConstructors   : Map[InternalName, MethodNameAndType]

  def jliLambdaMetaFactoryMetafactoryHandle    : Handle
  def jliLambdaMetaFactoryAltMetafactoryHandle : Handle
  def jliLambdaDeserializeBootstrapHandle      : Handle
  def jliStringConcatFactoryMakeConcatWithConstantsHandle: Handle

  def asmBoxTo  : Map[BType, MethodNameAndType]
  def asmUnboxTo: Map[BType, MethodNameAndType]

  def typeOfArrayOp: Map[Int, BType]

  // Concurrent maps because stack map frames are computed when in the class writer, which
  // might run on multiple classes concurrently.
  private val classBTypeCache: Lazy[ConcurrentHashMap[InternalName, ClassBType]] =
    frontendAccess.perRunLazy(new ConcurrentHashMap[InternalName, ClassBType])

  /** See doc of ClassBType.apply. This is where to use that method from. */
  def classBType[T](internalName: InternalName, t: T, fromSymbol: Boolean)(init: (ClassBType, T) => Either[NoClassBTypeInfo, ClassInfo]): ClassBType =
    ClassBType(internalName, t, fromSymbol, this, classBTypeCache.get)(init)

  /** Obtain a previously constructed ClassBType for a given internal name. */
  def classBTypeFromInternalName(internalName: InternalName): ClassBType =
    classBTypeCache.get.get(internalName)

}