package dotty.tools
package backend
package jvm

import java.util.concurrent.ConcurrentHashMap
import BTypes.InternalName
import BackendReporting.NoClassBTypeInfo
import dotty.tools.dotc.core.Symbols.{Symbol, defn}
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.backend.jvm.PostProcessorFrontendAccess.Lazy
import dotty.tools.dotc.core.Flags.{JavaDefined, Method, ModuleClass}
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Types.{AnnotatedType, JavaArrayType, RefinedType, SingletonType, ThisType, Type, TypeRef}
import dotty.tools.dotc.report

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

  def classBTypeFromSymbol(classSym: Symbol): ClassBType
  def mirrorClassBTypeFromSymbol(moduleClassSym: Symbol): ClassBType

  /**
   * The class internal name for a given class symbol.
   */
  final def internalName(sym: Symbol)(using Context): String = {
    // For each java class, the scala compiler creates a class and a module (thus a module class).
    // If the `sym` is a java module class, we use the java class instead. This ensures that the
    // ClassBType is created from the main class (instead of the module class).
    // The two symbols have the same name, so the resulting internalName is the same.
    val classSym = if (sym.is(JavaDefined) && sym.is(ModuleClass)) sym.linkedClass else sym
    getClassBType(classSym).internalName
  }

  private def assertClassNotArray(sym: Symbol)(using Context): Unit = {
    assert(sym.isClass, sym)
    assert(sym != defn.ArrayClass || BackendUtils.compilingArray, sym)
  }

  private def assertClassNotArrayNotPrimitive(sym: Symbol)(using Context): Unit = {
    assertClassNotArray(sym)
    assert(!primitiveTypeMap.contains(sym) || BackendUtils.compilingPrimitive, sym)
  }

  /**
   * The ClassBType for a class symbol.
   *
   * The class symbol scala.Nothing is mapped to the class scala.runtime.Nothing$. Similarly,
   * scala.Null is mapped to scala.runtime.Null$. This is because there exist no class files
   * for the Nothing / Null. If used for example as a parameter type, we use the runtime classes
   * in the classfile method signature.
   *
   * Note that the referenced class symbol may be an implementation class. For example when
   * compiling a mixed-in method that forwards to the static method in the implementation class,
   * the class descriptor of the receiver (the implementation class) is obtained by creating the
   * ClassBType.
   */
  final def getClassBType(sym: Symbol)(using Context): ClassBType = {
    assertClassNotArrayNotPrimitive(sym)

    if (sym == defn.NothingClass) srNothingRef
    else if (sym == defn.NullClass) srNullRef
    else classBTypeFromSymbol(sym)
  }

  /*
   * must-single-thread
   */
  final def asmMethodType(msym: Symbol)(using Context): MethodBType = {
    assert(msym.is(Method), s"not a method-symbol: $msym")
    val resT: BType =
      if (msym.isClassConstructor || msym.isConstructor) UNIT
      else toTypeKind(msym.info.resultType)
    MethodBType(msym.info.firstParamTypes.map(toTypeKind), resT)
  }

  /**
   * The jvm descriptor of a type.
   */
  final def typeDescriptor(t: Type): String = {
    toTypeKind(t).descriptor
  }

  /**
   * The jvm descriptor for a symbol.
   */
  final def symDescriptor(sym: Symbol)(using Context): String = getClassBType(sym).descriptor

  final def toTypeKind(tp: Type)(using Context): BType = typeToTypeKind(tp)

  /**
   * This method returns the BType for a type reference, for example a parameter type.
   *
   * If the result is a ClassBType for a nested class, it is added to the innerClassBufferASM.
   *
   * If `t` references a class, toTypeKind ensures that the class is not an implementation class.
   * See also comment on getClassBTypeAndRegisterInnerClass, which is invoked for implementation
   * classes.
   */
  final def typeToTypeKind(tp: Type)(using Context): BType = {
    val defn = ctx.definitions

    /**
     * Primitive types are represented as TypeRefs to the class symbol of, for example, scala.Int.
     * The `primitiveTypeMap` maps those class symbols to the corresponding PrimitiveBType.
     */
    def primitiveOrClassToBType(sym: Symbol): BType = {
      assert(sym.isClass, sym)
      assert(sym != defn.ArrayClass || BackendUtils.compilingArray, sym)
      primitiveTypeMap.getOrElse(sym, getClassBType(sym))
    }

    /**
     * When compiling Array.scala, the type parameter T is not erased and shows up in method
     * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
     */
    def nonClassTypeRefToBType(sym: Symbol): ClassBType = {
      assert(sym.isType && BackendUtils.compilingArray, sym)
      ObjectRef
    }

    tp.widenDealias match {
      case JavaArrayType(el) => ArrayBType(typeToTypeKind(el)) // Array type such as Array[Int] (kept by erasure)
      case t: TypeRef =>
        t.info match {

          case _ =>
            if (!t.symbol.isClass) nonClassTypeRefToBType(t.symbol) // See comment on nonClassTypeRefToBType
            else primitiveOrClassToBType(t.symbol) // Common reference to a type such as scala.Int or java.lang.String
        }
      case Types.ClassInfo(_, sym, _, _, _) => primitiveOrClassToBType(sym) // We get here, for example, for genLoadModule, which invokes toTypeKind(moduleClassSymbol.info)

      /* AnnotatedType should (probably) be eliminated by erasure. However, we know it happens for
        * meta-annotated annotations (@(ann @getter) val x = 0), so we don't emit a warning.
        * The type in the AnnotationInfo is an AnnotatedTpe. Tested in jvm/annotations.scala.
        */
      case a@AnnotatedType(t, _) =>
        report.debuglog(s"typeKind of annotated type $a")
        typeToTypeKind(t)

      /* The cases below should probably never occur. They are kept for now to avoid introducing
        * new compiler crashes, but we added a warning. The compiler / library bootstrap and the
        * test suite don't produce any warning.
        */

      case tp =>
        report.warning(
          s"an unexpected type representation reached the compiler backend while compiling ${ctx.compilationUnit}: $tp. " +
            "If possible, please file a bug on https://github.com/scala/scala3/issues")

        tp match {
          case tp: ThisType if tp.cls == defn.ArrayClass => ObjectRef // was introduced in 9b17332f11 to fix SI-999, but this code is not reached in its test, or any other test
          case tp: ThisType => getClassBType(tp.cls)
          // case t: SingletonType                   => primitiveOrClassToBType(t.classSymbol)
          case t: SingletonType => typeToTypeKind(t.underlying)
          case t: RefinedType => typeToTypeKind(t.parent)
        }
    }
  }
}