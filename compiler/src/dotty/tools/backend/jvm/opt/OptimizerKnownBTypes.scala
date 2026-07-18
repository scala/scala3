package dotty.tools.backend.jvm.opt

import dotty.tools.backend.jvm.BTypes.InternalName
import dotty.tools.backend.jvm.{BType, BTypeLoader, ClassBType, KnownBTypes, MethodBType, UNIT}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.{defn, requiredClass}

import scala.annotation.constructorOnly

case class MethodNameAndType(name: String, methodType: MethodBType)

final class OptimizerKnownBTypes(ts: BTypeLoader)(using @constructorOnly initctx: Context) extends KnownBTypes(ts) {

  val srNothingRef: ClassBType = ts.classBTypeFromSymbol(defn.RuntimeNothingClass)
  val srNullRef: ClassBType = ts.classBTypeFromSymbol(defn.RuntimeNullClass)

  val boxedClasses: Set[ClassBType] = boxedClassOfPrimitive.values.toSet

  val srBoxedUnitRef: ClassBType = ts.classBTypeFromSymbol(requiredClass[scala.runtime.BoxedUnit])

  val PredefRef: ClassBType = ts.classBTypeFromSymbol(defn.ScalaPredefModuleClass)

  val jlCloneableRef: ClassBType = ts.classBTypeFromSymbol(defn.JavaCloneableClass)

  val jiSerializableRef: ClassBType = ts.classBTypeFromSymbol(requiredClass[java.io.Serializable])

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
