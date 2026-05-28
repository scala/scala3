package dotty.tools.backend.jvm

import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.transform.Erasure

import scala.tools.asm.{Handle, Opcodes}
import dotty.tools.dotc.core.Symbols
import BTypes.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.*

import scala.annotation.constructorOnly

class KnownBTypes(loader: BTypeLoader)(using @constructorOnly initctx: Context) {
  val ObjectRef: ClassBType = loader.classBTypeFromSymbol(defn.ObjectClass)
  val StringRef: ClassBType = loader.classBTypeFromSymbol(defn.StringClass)
  val srNullRef: ClassBType = loader.classBTypeFromSymbol(defn.RuntimeNullClass)
  val jlClassRef: ClassBType = loader.classBTypeFromSymbol(defn.ClassClass)

  val jlThrowableRef: ClassBType = loader.classBTypeFromSymbol(defn.ThrowableClass)
  val jlClassCastExceptionRef: ClassBType = loader.classBTypeFromSymbol(defn.ClassCastExceptionClass)

  val srBoxesRuntimeRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[scala.runtime.BoxesRunTime])

  protected val jliLambdaMetafactoryRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.LambdaMetafactory])
  protected val jliStringConcatFactoryRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.StringConcatFactory])
  protected val jliMethodHandlesLookupRef: ClassBType = loader.classBTypeFromSymbol(defn.MethodHandlesLookupClass)
  protected val jliMethodTypeRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType])
  protected val jliMethodHandleRef: ClassBType = loader.classBTypeFromSymbol(defn.MethodHandleClass)
  protected val jliCallSiteRef: ClassBType = loader.classBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite])
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

  /**
   * Maps the method symbol for a box method to the boxed type of the result. For example, the
   * method symbol for `Byte.box()` is mapped to the ClassBType `java/lang/Byte`.
   */
  val boxResultType: Map[Symbol, ClassBType] = Map(
    Erasure.Boxing.boxMethod(defn.UnitClass)    -> loader.classBTypeFromSymbol(requiredClass[java.lang.Void]),
    Erasure.Boxing.boxMethod(defn.BooleanClass) -> loader.classBTypeFromSymbol(requiredClass[java.lang.Boolean]),
    Erasure.Boxing.boxMethod(defn.ByteClass)    -> loader.classBTypeFromSymbol(requiredClass[java.lang.Byte]),
    Erasure.Boxing.boxMethod(defn.ShortClass)   -> loader.classBTypeFromSymbol(requiredClass[java.lang.Short]),
    Erasure.Boxing.boxMethod(defn.CharClass)    -> loader.classBTypeFromSymbol(requiredClass[java.lang.Character]),
    Erasure.Boxing.boxMethod(defn.IntClass)     -> loader.classBTypeFromSymbol(requiredClass[java.lang.Integer]),
    Erasure.Boxing.boxMethod(defn.LongClass)    -> loader.classBTypeFromSymbol(requiredClass[java.lang.Long]),
    Erasure.Boxing.boxMethod(defn.FloatClass)   -> loader.classBTypeFromSymbol(requiredClass[java.lang.Float]),
    Erasure.Boxing.boxMethod(defn.DoubleClass)  -> loader.classBTypeFromSymbol(requiredClass[java.lang.Double])
  )
  /**
   * Maps the method symbol for an unbox method to the primitive type of the result.
   * For example, the method symbol for `Byte.unbox()` is mapped to the PrimitiveBType BYTE.
   */
  val unboxResultType: Map[Symbol, BType] = Map(
    Erasure.Boxing.unboxMethod(defn.UnitClass)    -> UNIT,
    Erasure.Boxing.unboxMethod(defn.BooleanClass) -> BOOL,
    Erasure.Boxing.unboxMethod(defn.ByteClass)    -> BYTE,
    Erasure.Boxing.unboxMethod(defn.ShortClass)   -> SHORT,
    Erasure.Boxing.unboxMethod(defn.CharClass)    -> CHAR,
    Erasure.Boxing.unboxMethod(defn.IntClass)     -> INT,
    Erasure.Boxing.unboxMethod(defn.LongClass)    -> LONG,
    Erasure.Boxing.unboxMethod(defn.FloatClass)   -> FLOAT,
    Erasure.Boxing.unboxMethod(defn.DoubleClass)  -> DOUBLE
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

  import dotty.tools.backend.ScalaPrimitivesOps.*
  val typeOfArrayOp: Map[Int, BType] = Map(
    ZARRAY_LENGTH -> BOOL, ZARRAY_GET -> BOOL, ZARRAY_SET -> BOOL,
    BARRAY_LENGTH -> BYTE, BARRAY_GET -> BYTE, BARRAY_SET -> BYTE,
    SARRAY_LENGTH -> SHORT, SARRAY_GET -> SHORT, SARRAY_SET -> SHORT,
    CARRAY_LENGTH -> CHAR, CARRAY_GET -> CHAR, CARRAY_SET -> CHAR,
    IARRAY_LENGTH -> INT, IARRAY_GET -> INT, IARRAY_SET -> INT,
    LARRAY_LENGTH -> LONG, LARRAY_GET -> LONG, LARRAY_SET -> LONG,
    FARRAY_LENGTH -> FLOAT, FARRAY_GET -> FLOAT, FARRAY_SET -> FLOAT,
    DARRAY_LENGTH -> DOUBLE, DARRAY_GET -> DOUBLE, DARRAY_SET -> DOUBLE,
    OARRAY_LENGTH -> ObjectRef, OARRAY_GET -> ObjectRef, OARRAY_SET -> ObjectRef
  )
}


case class MethodNameAndType(name: String, methodType: MethodBType)

final class WellKnownBTypes(ts: BTypeLoader)(using @constructorOnly initctx: Context) extends KnownBTypes(ts) {

  val srNothingRef: ClassBType = ts.classBTypeFromSymbol(defn.RuntimeNothingClass)

  val boxedClasses: Set[ClassBType] = boxedClassOfPrimitive.values.toSet

  val srBoxedUnitRef: ClassBType = ts.classBTypeFromSymbol(requiredClass[scala.runtime.BoxedUnit])

  val PredefRef: ClassBType = ts.classBTypeFromSymbol(defn.ScalaPredefModuleClass)

  val jlCloneableRef: ClassBType = ts.classBTypeFromSymbol(defn.JavaCloneableClass)

  val jiSerializableRef: ClassBType = ts.classBTypeFromSymbol(requiredClass[java.io.Serializable])

  val jlIllegalArgExceptionRef: ClassBType = ts.classBTypeFromSymbol(requiredClass[java.lang.IllegalArgumentException])

  val jliSerializedLambdaRef: ClassBType = ts.classBTypeFromSymbol(requiredClass[java.lang.invoke.SerializedLambda])

  private val srLambdaDeserialize: ClassBType = ts.classBTypeFromSymbol(requiredClass[scala.runtime.LambdaDeserialize])


  val jliLambdaDeserializeBootstrapHandle: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    srLambdaDeserialize.internalName,
    "bootstrap",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(jliMethodHandleRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)

  // java/lang/Boolean -> MethodNameAndType(valueOf,(Z)Ljava/lang/Boolean;)
  val javaBoxMethods: Map[InternalName, MethodNameAndType] = _javaBoxMethods(using initctx)
  private def _javaBoxMethods(using Context): Map[InternalName, MethodNameAndType] = {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val boxed = defn.boxedClass(primitive)
      val unboxed = ts.bTypeFromSymbol(primitive)
      val method = MethodNameAndType("valueOf", MethodBType(List(unboxed), boxedClassOfPrimitive(unboxed)))
      (ts.classBTypeFromSymbol(boxed).internalName, method)
    }))
  }

  // java/lang/Boolean -> MethodNameAndType(booleanValue,()Z)
  val javaUnboxMethods: Map[InternalName, MethodNameAndType] = _javaUnboxMethods(using initctx)
  private def _javaUnboxMethods(using Context): Map[InternalName, MethodNameAndType] = {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val boxed = defn.boxedClass(primitive)
      val name = primitive.name.toString.toLowerCase + "Value"
      (ts.classBTypeFromSymbol(boxed).internalName, MethodNameAndType(name, MethodBType(Nil, ts.bTypeFromSymbol(primitive))))
    }))
  }

  private def predefBoxingMethods(isBox: Boolean, getName: (String, String) => String)(using Context): Map[String, MethodBType] =
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val unboxed = ts.bTypeFromSymbol(primitive)
      val boxed = boxedClassOfPrimitive(unboxed)
      val name = getName(primitive.name.toString, defn.boxedClass(primitive).name.toString)
      (name, MethodBType(List(if isBox then unboxed else boxed), if isBox then boxed else unboxed))
    }))

  // boolean2Boolean -> (Z)Ljava/lang/Boolean;
  val predefAutoBoxMethods: Map[String, MethodBType] = _predefAutoBoxMethods(using initctx)
  private def _predefAutoBoxMethods(using Context): Map[String, MethodBType] = predefBoxingMethods(true, (primitive, boxed) => primitive.toLowerCase + "2" + boxed)

  // Boolean2boolean -> (Ljava/lang/Boolean;)Z
  val predefAutoUnboxMethods: Map[String, MethodBType] = _predefAutoUnboxMethods(using initctx)
  private def _predefAutoUnboxMethods(using Context): Map[String, MethodBType] = predefBoxingMethods(false, (primitive, boxed) => boxed + "2" + primitive.toLowerCase)

  // scala/runtime/BooleanRef -> MethodNameAndType(create,(Z)Lscala/runtime/BooleanRef;)
  val srRefCreateMethods: Map[InternalName, MethodNameAndType] = _srRefCreateMethods(using initctx)
  private def _srRefCreateMethods(using Context): Map[InternalName, MethodNameAndType] = {
    Map.from(defn.ScalaValueClassesNoUnit().union(Set(defn.ObjectClass)).flatMap(primitive => {
      val boxed = if primitive == defn.ObjectClass then primitive else defn.boxedClass(primitive)
      val unboxed = if primitive == defn.ObjectClass then ObjectRef else ts.bTypeFromSymbol(primitive)
      val refClass = Symbols.requiredClass("scala.runtime." + primitive.name.toString + "Ref")
      val volatileRefClass = Symbols.requiredClass("scala.runtime.Volatile" + primitive.name.toString + "Ref")
      List(
        (ts.classBTypeFromSymbol(refClass).internalName, MethodNameAndType(nme.create.toString, MethodBType(List(unboxed), ts.bTypeFromSymbol(refClass)))),
        (ts.classBTypeFromSymbol(volatileRefClass).internalName, MethodNameAndType(nme.create.toString, MethodBType(List(unboxed), ts.bTypeFromSymbol(volatileRefClass))))
      )
    }))
  }

  // scala/runtime/BooleanRef -> MethodNameAndType(zero,()Lscala/runtime/BooleanRef;)
  val srRefZeroMethods: Map[InternalName, MethodNameAndType] = _srRefZeroMethods(using initctx)
  private def _srRefZeroMethods(using Context): Map[InternalName, MethodNameAndType] = {
    Map.from(defn.ScalaValueClassesNoUnit().union(Set(defn.ObjectClass)).flatMap(primitive => {
      val boxed = if primitive == defn.ObjectClass then primitive else defn.boxedClass(primitive)
      val refClass = Symbols.requiredClass("scala.runtime." + primitive.name.toString + "Ref")
      val volatileRefClass = Symbols.requiredClass("scala.runtime.Volatile" + primitive.name.toString + "Ref")
      List(
        (ts.classBTypeFromSymbol(refClass).internalName, MethodNameAndType(nme.zero.toString, MethodBType(List(), ts.bTypeFromSymbol(refClass)))),
        (ts.classBTypeFromSymbol(volatileRefClass).internalName, MethodNameAndType(nme.zero.toString, MethodBType(List(), ts.bTypeFromSymbol(volatileRefClass))))
      )
    }))
  }

  // java/lang/Boolean -> MethodNameAndType(<init>,(Z)V)
  val primitiveBoxConstructors: Map[InternalName, MethodNameAndType] = _primitiveBoxConstructors(using initctx)
  private def _primitiveBoxConstructors(using Context): Map[InternalName, MethodNameAndType] = {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val boxed = defn.boxedClass(primitive)
      val unboxed = ts.bTypeFromSymbol(primitive)
      (ts.classBTypeFromSymbol(boxed).internalName, MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(unboxed), UNIT)))
    }))
  }

  // Z -> MethodNameAndType(boxToBoolean,(Z)Ljava/lang/Boolean;)
  val srBoxesRuntimeBoxToMethods: Map[BType, MethodNameAndType] = _srBoxesRuntimeBoxToMethods(using initctx)
  private def _srBoxesRuntimeBoxToMethods(using Context): Map[BType, MethodNameAndType] = {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val bType = ts.bTypeFromSymbol(primitive)
      val boxed = boxedClassOfPrimitive(bType)
      val name = "boxTo" + defn.boxedClass(primitive).name.toString
      (bType, MethodNameAndType(name, MethodBType(List(bType), boxed)))
    }))
  }

  // Z -> MethodNameAndType(unboxToBoolean,(Ljava/lang/Object;)Z)
  val srBoxesRuntimeUnboxToMethods: Map[BType, MethodNameAndType] = _srBoxesRuntimeUnboxToMethods(using initctx)
  private def _srBoxesRuntimeUnboxToMethods(using Context): Map[BType, MethodNameAndType] = {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val bType = ts.bTypeFromSymbol(primitive)
      val name = "unboxTo" + primitive.name.toString
      (bType, MethodNameAndType(name, MethodBType(List(ObjectRef), bType)))
    }))
  }

  // scala/runtime/BooleanRef -> MethodNameAndType(<init>,(Z)V)
  val srRefConstructors: Map[InternalName, MethodNameAndType] = _srRefConstructors(using initctx)
  private def _srRefConstructors(using Context): Map[InternalName, MethodNameAndType] = {
    Map.from(defn.ScalaValueClassesNoUnit().union(Set(defn.ObjectClass)).flatMap(primitive => {
      val boxed = if primitive == defn.ObjectClass then primitive else defn.boxedClass(primitive)
      val unboxed = if primitive == defn.ObjectClass then ObjectRef else ts.bTypeFromSymbol(primitive)
      val refClass = Symbols.requiredClass("scala.runtime." + primitive.name.toString + "Ref")
      val volatileRefClass = Symbols.requiredClass("scala.runtime.Volatile" + primitive.name.toString + "Ref")
      List(
        (ts.classBTypeFromSymbol(refClass).internalName, MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(unboxed), UNIT))),
        (ts.classBTypeFromSymbol(volatileRefClass).internalName, MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(unboxed), UNIT)))
      )
    }))
  }

  // scala/Tuple3 -> MethodNameAndType(<init>,(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V)
  // scala/Tuple2$mcZC$sp -> MethodNameAndType(<init>,(ZC)V)
  // ... this was easy in scala2, but now we don't specialize them so we have to know each name
  // tuple1 is specialized for D, I, J
  // tuple2 is specialized for C, D, I, J, Z in each parameter
  val tupleClassConstructors: Map[InternalName, MethodNameAndType] = _tupleClassConstructors(using initctx)
  private def _tupleClassConstructors(using Context): Map[InternalName, MethodNameAndType] = {
    val spec1 = List(defn.DoubleClass, defn.IntClass, defn.LongClass)
    val spec2 = List(defn.CharClass, defn.DoubleClass, defn.IntClass, defn.LongClass, defn.BooleanClass)
    Map.from(
      Iterator.concat(
        (1 to 22).map { n =>
          ("scala/Tuple" + n, MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List.fill(n)(ObjectRef), UNIT)))
        },
        spec1.map { sp1 =>
          val prim = ts.bTypeFromSymbol(sp1)
          ("scala/Tuple1$mc" + prim.descriptor + "$sp", MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(), UNIT)))
        },
        for sp2a <- spec2; sp2b <- spec2 yield {
          val primA = ts.bTypeFromSymbol(sp2a)
          val primB = ts.bTypeFromSymbol(sp2b)
          ("scala/Tuple2$mc" + primA.descriptor + primB.descriptor + "$sp", MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(primA, primB), UNIT)))
        }
      )
    )
  }
}
