package dotty.tools.backend.jvm

import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.transform.Erasure

import scala.tools.asm.{Handle, Opcodes}
import dotty.tools.dotc.core.Symbols
import BTypes.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.*
import PostProcessorFrontendAccess.Lazy


case class MethodNameAndType(name: String, methodType: MethodBType)

final class WellKnownBTypes(ppa: PostProcessorFrontendAccess, ts: BTypeLoader)(using Context) {

  def ObjectRef: ClassBType = _ObjectRef.get
  private lazy val _ObjectRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(defn.ObjectClass))

  def srNothingRef: ClassBType = _srNothingRef.get
  private lazy val _srNothingRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(defn.RuntimeNothingClass))

  def srNullRef: ClassBType = _srNullRef.get
  private lazy val _srNullRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(defn.RuntimeNullClass))

  /**
   * Map from primitive types to their boxed class type. Useful when pushing class literals onto the
   * operand stack (ldc instruction taking a class literal), see genConstant.
   */
  def boxedClassOfPrimitive: Map[BType, ClassBType] = _boxedClassOfPrimitive.get
  private lazy val _boxedClassOfPrimitive: Lazy[Map[BType, ClassBType]] = ppa.perRunLazy(Map(
    UNIT   -> ts.classBTypeFromSymbol(requiredClass[java.lang.Void]),
    BOOL   -> ts.classBTypeFromSymbol(requiredClass[java.lang.Boolean]),
    BYTE   -> ts.classBTypeFromSymbol(requiredClass[java.lang.Byte]),
    SHORT  -> ts.classBTypeFromSymbol(requiredClass[java.lang.Short]),
    CHAR   -> ts.classBTypeFromSymbol(requiredClass[java.lang.Character]),
    INT    -> ts.classBTypeFromSymbol(requiredClass[java.lang.Integer]),
    LONG   -> ts.classBTypeFromSymbol(requiredClass[java.lang.Long]),
    FLOAT  -> ts.classBTypeFromSymbol(requiredClass[java.lang.Float]),
    DOUBLE -> ts.classBTypeFromSymbol(requiredClass[java.lang.Double])
  ))

  lazy val boxedClasses: Set[ClassBType] = boxedClassOfPrimitive.values.toSet

  /**
   * Maps the method symbol for a box method to the boxed type of the result. For example, the
   * method symbol for `Byte.box()` is mapped to the ClassBType `java/lang/Byte`.
   */
  def boxResultType: Map[Symbol, ClassBType] = _boxResultType.get
  private lazy val _boxResultType: Lazy[Map[Symbol, ClassBType]] = ppa.perRunLazy{
    val boxMethods = defn.ScalaValueClasses().map{x =>
      (x, Erasure.Boxing.boxMethod(x.asClass))
    }.toMap
    for ((valueClassSym, boxMethodSym) <- boxMethods)
      yield boxMethodSym -> boxedClassOfPrimitive(ts.bTypeFromSymbol(valueClassSym))
  }

  /**
   * Maps the method symbol for an unbox method to the primitive type of the result.
   * For example, the method symbol for `Byte.unbox()` is mapped to the PrimitiveBType BYTE. */
  def unboxResultType: Map[Symbol, BType] = _unboxResultType.get
  private lazy val _unboxResultType = ppa.perRunLazy[Map[Symbol, BType]]{
    val unboxMethods: Map[Symbol, Symbol] =
      defn.ScalaValueClasses().map(x => (x, Erasure.Boxing.unboxMethod(x.asClass))).toMap
    for ((valueClassSym, unboxMethodSym) <- unboxMethods)
      yield unboxMethodSym -> ts.bTypeFromSymbol(valueClassSym)
  }

  def srBoxedUnitRef: ClassBType = _srBoxedUnitRef.get
  private lazy val _srBoxedUnitRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[scala.runtime.BoxedUnit]))

  def StringRef: ClassBType = _StringRef.get
  private lazy val _StringRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(defn.StringClass))

  def PredefRef: ClassBType = _PredefRef.get
  private lazy val _PredefRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(defn.ScalaPredefModuleClass))

  def jlClassRef: ClassBType = _jlClassRef.get
  private lazy val _jlClassRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[java.lang.Class[?]]))

  def jlThrowableRef: ClassBType = _jlThrowableRef.get
  private lazy val _jlThrowableRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(defn.ThrowableClass))

  def jlCloneableRef: ClassBType = _jlCloneableRef.get
  private lazy val _jlCloneableRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(defn.JavaCloneableClass))

  def jiSerializableRef: ClassBType = _jiSerializableRef.get
  private lazy val _jiSerializableRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[java.io.Serializable]))

  def jlClassCastExceptionRef: ClassBType = _jlClassCastExceptionRef.get
  private lazy val _jlClassCastExceptionRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[java.lang.ClassCastException]))

  def jlIllegalArgExceptionRef: ClassBType = _jlIllegalArgExceptionRef.get
  private lazy val _jlIllegalArgExceptionRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[java.lang.IllegalArgumentException]))

  def jliSerializedLambdaRef: ClassBType = _jliSerializedLambdaRef.get
  private lazy val _jliSerializedLambdaRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[java.lang.invoke.SerializedLambda]))

  def srBoxesRuntimeRef: ClassBType = _srBoxesRuntimeRef.get
  private lazy val _srBoxesRuntimeRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[scala.runtime.BoxesRunTime]))

  private def jliCallSiteRef: ClassBType = _jliCallSiteRef.get
  private lazy val _jliCallSiteRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[java.lang.invoke.CallSite]))

  private def jliLambdaMetafactoryRef: ClassBType = _jliLambdaMetafactoryRef.get
  private lazy val _jliLambdaMetafactoryRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[java.lang.invoke.LambdaMetafactory]))

  private def jliMethodHandleRef: ClassBType = _jliMethodHandleRef.get
  private lazy val _jliMethodHandleRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(defn.MethodHandleClass))

  private def jliMethodHandlesLookupRef: ClassBType = _jliMethodHandlesLookupRef.get
  private lazy val _jliMethodHandlesLookupRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(defn.MethodHandlesLookupClass))

  private def jliMethodTypeRef: ClassBType = _jliMethodTypeRef.get
  private lazy val _jliMethodTypeRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[java.lang.invoke.MethodType]))

  private def jliStringConcatFactoryRef: ClassBType = _jliStringConcatFactoryRef.get
  private lazy val _jliStringConcatFactoryRef: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[java.lang.invoke.StringConcatFactory]))

  private def srLambdaDeserialize: ClassBType = _srLambdaDeserialize.get
  private lazy val _srLambdaDeserialize: Lazy[ClassBType] = ppa.perRunLazy(ts.classBTypeFromSymbol(requiredClass[scala.runtime.LambdaDeserialize]))


  def jliLambdaMetaFactoryMetafactoryHandle: Handle = _jliLambdaMetaFactoryMetafactoryHandle.get
  private lazy val _jliLambdaMetaFactoryMetafactoryHandle: Lazy[Handle] = ppa.perRunLazy{new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "metafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, jliMethodTypeRef, jliMethodHandleRef, jliMethodTypeRef),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  def jliLambdaMetaFactoryAltMetafactoryHandle: Handle = _jliLambdaMetaFactoryAltMetafactoryHandle.get
  private lazy val _jliLambdaMetaFactoryAltMetafactoryHandle: Lazy[Handle] = ppa.perRunLazy{ new Handle(
    Opcodes.H_INVOKESTATIC,
    jliLambdaMetafactoryRef.internalName,
    "altMetafactory",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(ObjectRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  def jliLambdaDeserializeBootstrapHandle: Handle = _jliLambdaDeserializeBootstrapHandle.get
  private lazy val _jliLambdaDeserializeBootstrapHandle: Lazy[Handle] = ppa.perRunLazy{ new Handle(
    Opcodes.H_INVOKESTATIC,
    srLambdaDeserialize.internalName,
    "bootstrap",
    MethodBType(
      List(jliMethodHandlesLookupRef, StringRef, jliMethodTypeRef, ArrayBType(jliMethodHandleRef)),
      jliCallSiteRef
    ).descriptor,
    /* itf = */ false)}

  def jliStringConcatFactoryMakeConcatWithConstantsHandle: Handle = _jliStringConcatFactoryMakeConcatWithConstantsHandle.get
  private lazy val _jliStringConcatFactoryMakeConcatWithConstantsHandle: Lazy[Handle] = ppa.perRunLazy{ new Handle(
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

  // java/lang/Boolean -> MethodNameAndType(valueOf,(Z)Ljava/lang/Boolean;)
  def javaBoxMethods: Map[InternalName, MethodNameAndType] = _javaBoxMethods.get
  private lazy val _javaBoxMethods: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val boxed = defn.boxedClass(primitive)
      val unboxed = ts.bTypeFromSymbol(primitive)
      val method = MethodNameAndType("valueOf", MethodBType(List(unboxed), boxedClassOfPrimitive(unboxed)))
      (ts.classBTypeFromSymbol(boxed).internalName, method)
    }))
  }

  // java/lang/Boolean -> MethodNameAndType(booleanValue,()Z)
  def javaUnboxMethods: Map[InternalName, MethodNameAndType] = _javaUnboxMethods.get
  private lazy val _javaUnboxMethods: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val boxed = defn.boxedClass(primitive)
      val name = primitive.name.toString.toLowerCase + "Value"
      (ts.classBTypeFromSymbol(boxed).internalName, MethodNameAndType(name, MethodBType(Nil, ts.bTypeFromSymbol(primitive))))
    }))
  }

  private def predefBoxingMethods(isBox: Boolean, getName: (String, String) => String): Map[String, MethodBType] =
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val unboxed = ts.bTypeFromSymbol(primitive)
      val boxed = boxedClassOfPrimitive(unboxed)
      val name = getName(primitive.name.toString, defn.boxedClass(primitive).name.toString)
      (name, MethodBType(List(if isBox then unboxed else boxed), if isBox then boxed else unboxed))
    }))

  // boolean2Boolean -> (Z)Ljava/lang/Boolean;
  def predefAutoBoxMethods: Map[String, MethodBType] = _predefAutoBoxMethods.get
  private lazy val _predefAutoBoxMethods: Lazy[Map[String, MethodBType]] = ppa.perRunLazy(predefBoxingMethods(true, (primitive, boxed) => primitive.toLowerCase + "2" + boxed))

  // Boolean2boolean -> (Ljava/lang/Boolean;)Z
  def predefAutoUnboxMethods: Map[String, MethodBType] = _predefAutoUnboxMethods.get
  private lazy val _predefAutoUnboxMethods: Lazy[Map[String, MethodBType]] = ppa.perRunLazy(predefBoxingMethods(false, (primitive, boxed) => boxed + "2" + primitive.toLowerCase))

  // scala/runtime/BooleanRef -> MethodNameAndType(create,(Z)Lscala/runtime/BooleanRef;)
  def srRefCreateMethods: Map[InternalName, MethodNameAndType] = _srRefCreateMethods.get
  private lazy val _srRefCreateMethods: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
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
  def srRefZeroMethods: Map[InternalName, MethodNameAndType] = _srRefZeroMethods.get
  private lazy val _srRefZeroMethods: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
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
  def primitiveBoxConstructors: Map[InternalName, MethodNameAndType] = _primitiveBoxConstructors.get
  private lazy val _primitiveBoxConstructors: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val boxed = defn.boxedClass(primitive)
      val unboxed = ts.bTypeFromSymbol(primitive)
      (ts.classBTypeFromSymbol(boxed).internalName, MethodNameAndType(nme.CONSTRUCTOR.toString, MethodBType(List(unboxed), UNIT)))
    }))
  }

  // Z -> MethodNameAndType(boxToBoolean,(Z)Ljava/lang/Boolean;)
  def srBoxesRuntimeBoxToMethods: Map[BType, MethodNameAndType] = _srBoxesRuntimeBoxToMethods.get
  private lazy val _srBoxesRuntimeBoxToMethods: Lazy[Map[BType, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val bType = ts.bTypeFromSymbol(primitive)
      val boxed = boxedClassOfPrimitive(bType)
      val name = "boxTo" + defn.boxedClass(primitive).name.toString
      (bType, MethodNameAndType(name, MethodBType(List(bType), boxed)))
    }))
  }

  // Z -> MethodNameAndType(unboxToBoolean,(Ljava/lang/Object;)Z)
  def srBoxesRuntimeUnboxToMethods: Map[BType, MethodNameAndType] = _srBoxesRuntimeUnboxToMethods.get
  private lazy val _srBoxesRuntimeUnboxToMethods: Lazy[Map[BType, MethodNameAndType]] = ppa.perRunLazy {
    Map.from(defn.ScalaValueClassesNoUnit().map(primitive => {
      val bType = ts.bTypeFromSymbol(primitive)
      val name = "unboxTo" + primitive.name.toString
      (bType, MethodNameAndType(name, MethodBType(List(ObjectRef), bType)))
    }))
  }

  // scala/runtime/BooleanRef -> MethodNameAndType(<init>,(Z)V)
  def srRefConstructors: Map[InternalName, MethodNameAndType] = _srRefConstructors.get
  private lazy val _srRefConstructors: Lazy[Map[InternalName, MethodNameAndType]] =  ppa.perRunLazy {
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
  def tupleClassConstructors: Map[InternalName, MethodNameAndType] = _tupleClassConstructors.get
  private lazy val _tupleClassConstructors: Lazy[Map[InternalName, MethodNameAndType]] = ppa.perRunLazy {
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
