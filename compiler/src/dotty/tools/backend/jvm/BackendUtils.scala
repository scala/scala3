package dotty.tools.backend.jvm

import scala.tools.asm
import scala.tools.asm.Handle
import scala.tools.asm.tree.InvokeDynamicInsnNode
import asm.tree.ClassNode
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import dotty.tools.dotc.report

import scala.language.unsafeNulls

/**
 * This component hosts tools and utilities used in the backend that require access to a `BTypes`
 * instance.
 */
class BackendUtils(val postProcessor: PostProcessor) {
  import postProcessor.{bTypes, frontendAccess}
  import frontendAccess.{compilerSettings}
  import bTypes.*
  import coreBTypes.jliLambdaMetaFactoryAltMetafactoryHandle

  lazy val classfileVersion: Int = BackendUtils.classfileVersionMap(compilerSettings.target.toInt)

  lazy val extraProc: Int = {
    import GenBCodeOps.addFlagIf
    val majorVersion: Int = (classfileVersion & 0xFF)
    val emitStackMapFrame = (majorVersion >= 50)
    asm.ClassWriter.COMPUTE_MAXS
      .addFlagIf(emitStackMapFrame, asm.ClassWriter.COMPUTE_FRAMES)
  }

  def collectSerializableLambdas(classNode: ClassNode): Array[Handle] = {
    val indyLambdaBodyMethods = new mutable.ArrayBuffer[Handle]
    for (m <- classNode.methods.asScala) {
      val iter = m.instructions.iterator
      while (iter.hasNext) {
        val insn = iter.next()
        insn match {
          case indy: InvokeDynamicInsnNode
            if indy.bsm == jliLambdaMetaFactoryAltMetafactoryHandle =>
              import java.lang.invoke.LambdaMetafactory.FLAG_SERIALIZABLE
              val metafactoryFlags = indy.bsmArgs(3).asInstanceOf[Integer].toInt
              val isSerializable = (metafactoryFlags & FLAG_SERIALIZABLE) != 0
              if isSerializable then
                val implMethod = indy.bsmArgs(1).asInstanceOf[Handle]
                indyLambdaBodyMethods += implMethod
          case _ =>
        }
      }
    }
    indyLambdaBodyMethods.toArray
  }

  /*
  * Add:
  *
  * private static Object $deserializeLambda$(SerializedLambda l) {
  *   try return indy[scala.runtime.LambdaDeserialize.bootstrap, targetMethodGroup$0](l)
  *   catch {
  *     case i: IllegalArgumentException =>
  *       try return indy[scala.runtime.LambdaDeserialize.bootstrap, targetMethodGroup$1](l)
  *       catch {
  *         case i: IllegalArgumentException =>
  *           ...
  *             return indy[scala.runtime.LambdaDeserialize.bootstrap, targetMethodGroup${NUM_GROUPS-1}](l)
  *       }
  *
  * We use invokedynamic here to enable caching within the deserializer without needing to
  * host a static field in the enclosing class. This allows us to add this method to interfaces
  * that define lambdas in default methods.
  *
  * SI-10232 we can't pass arbitrary number of method handles to the final varargs parameter of the bootstrap
  * method due to a limitation in the JVM. Instead, we emit a separate invokedynamic bytecode for each group of target
  * methods.
  */
  def addLambdaDeserialize(classNode: ClassNode, implMethodsArray: Array[Handle]): Unit = {
    import asm.Opcodes.*
    import bTypes.*
    import coreBTypes.*

    val cw = classNode

    // Make sure to reference the ClassBTypes of all types that are used in the code generated
    // here (e.g. java/util/Map) are initialized. Initializing a ClassBType adds it to
    // `classBTypeFromInternalNameMap`. When writing the classfile, the asm ClassWriter computes
    // stack map frames and invokes the `getCommonSuperClass` method. This method expects all
    // ClassBTypes mentioned in the source code to exist in the map.

    val mv = cw.visitMethod(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$deserializeLambda$", serializedLamdaObjDesc, null, null)
    def emitLambdaDeserializeIndy(targetMethods: Seq[Handle]): Unit = {
      mv.visitVarInsn(ALOAD, 0)
      mv.visitInvokeDynamicInsn("lambdaDeserialize", serializedLamdaObjDesc, jliLambdaDeserializeBootstrapHandle, targetMethods*)
    }

    val targetMethodGroupLimit = 255 - 1 - 3 // JVM limit. See See MAX_MH_ARITY in CallSite.java
    val groups: Array[Array[Handle]] = implMethodsArray.grouped(targetMethodGroupLimit).toArray
    val numGroups = groups.length

    import scala.tools.asm.Label
    val initialLabels = Array.fill(numGroups - 1)(new Label())
    val terminalLabel = new Label
    def nextLabel(i: Int) = if (i == numGroups - 2) terminalLabel else initialLabels(i + 1)

    for ((label, i) <- initialLabels.iterator.zipWithIndex) {
      mv.visitTryCatchBlock(label, nextLabel(i), nextLabel(i), jlIllegalArgExceptionRef.internalName)
    }
    for ((label, i) <- initialLabels.iterator.zipWithIndex) {
      mv.visitLabel(label)
      emitLambdaDeserializeIndy(groups(i).toIndexedSeq)
      mv.visitInsn(ARETURN)
    }
    mv.visitLabel(terminalLabel)
    emitLambdaDeserializeIndy(groups(numGroups - 1).toIndexedSeq)
    mv.visitInsn(ARETURN)
  }

  private lazy val serializedLamdaObjDesc = {
    import coreBTypes.{ObjectRef, jliSerializedLambdaRef}
    MethodBType(jliSerializedLambdaRef :: Nil, ObjectRef).descriptor
  }

  /**
   * Visit the class node and collect all referenced nested classes.
   */
  def collectNestedClasses(classNode: ClassNode): (List[ClassBType], List[ClassBType]) = {
    // type InternalName = String
    val c = new NestedClassesCollector[ClassBType](nestedOnly = true) {
      def declaredNestedClasses(internalName: InternalName): List[ClassBType] =
        bTypes.classBTypeFromInternalName(internalName).info.memberClasses

      def getClassIfNested(internalName: InternalName): Option[ClassBType] = {
        val c = bTypes.classBTypeFromInternalName(internalName)
        Option.when(c.isNestedClass)(c)
      }

      def raiseError(msg: String, sig: String, e: Option[Throwable]): Unit = {
        // don't crash on invalid generic signatures
      }
    }
    c.visit(classNode)
    (c.declaredInnerClasses.toList, c.referredInnerClasses.toList)
  }

  /*
   * Populates the InnerClasses JVM attribute with `refedInnerClasses`. See also the doc on inner
   * classes in BTypes.scala.
   *
   * `refedInnerClasses` may contain duplicates, need not contain the enclosing inner classes of
   * each inner class it lists (those are looked up and included).
   *
   * This method serializes in the InnerClasses JVM attribute in an appropriate order,
   * not necessarily that given by `refedInnerClasses`.
   *
   * can-multi-thread
   */
  final def addInnerClasses(jclass: asm.ClassVisitor, declaredInnerClasses: List[ClassBType], refedInnerClasses: List[ClassBType]): Unit = {
    // sorting ensures nested classes are listed after their enclosing class thus satisfying the Eclipse Java compiler
    val allNestedClasses = new mutable.TreeSet[ClassBType]()(Ordering.by(_.internalName))
    allNestedClasses ++= declaredInnerClasses
    refedInnerClasses.foreach(allNestedClasses ++= _.enclosingNestedClassesChain)
    for nestedClass <- allNestedClasses
    do {
      // Extract the innerClassEntry - we know it exists, enclosingNestedClassesChain only returns nested classes.
      val Some(e) = nestedClass.innerClassAttributeEntry: @unchecked
      jclass.visitInnerClass(e.name, e.outerName, e.innerName, e.flags)
    }
  }
}

object BackendUtils {
  lazy val classfileVersionMap: Map[Int, Int] = Map(
    8 -> asm.Opcodes.V1_8,
    9 -> asm.Opcodes.V9,
    10 -> asm.Opcodes.V10,
    11 -> asm.Opcodes.V11,
    12 -> asm.Opcodes.V12,
    13 -> asm.Opcodes.V13,
    14 -> asm.Opcodes.V14,
    15 -> asm.Opcodes.V15,
    16 -> asm.Opcodes.V16,
    17 -> asm.Opcodes.V17,
    18 -> asm.Opcodes.V18,
    19 -> asm.Opcodes.V19,
    20 -> asm.Opcodes.V20,
    21 -> asm.Opcodes.V21,
    22 -> asm.Opcodes.V22,
    23 -> asm.Opcodes.V23,
    24 -> asm.Opcodes.V24
  )
}
