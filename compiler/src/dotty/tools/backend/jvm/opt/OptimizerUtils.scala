package dotty.tools.backend.jvm.opt

import dotty.tools.backend.jvm.BTypes.InternalName
import dotty.tools.backend.jvm.*
import dotty.tools.backend.jvm.analysis.AnalysisUtils
import dotty.tools.dotc.core.Definitions

import java.util.concurrent.ConcurrentHashMap
import scala.annotation.switch
import scala.collection.{BitSet, mutable}
import scala.jdk.CollectionConverters.*
import scala.tools.asm
import scala.tools.asm.tree.*
import scala.tools.asm.{Handle, Opcodes, Type}

/**
 * This component hosts tools and utilities used in the optimizer that require access to an `OptimizerKnownBTypes` instance.
 */
class OptimizerUtils(val ts: OptimizerKnownBTypes) {

  private val indyLambdaImplMethods: ConcurrentHashMap[InternalName, mutable.Map[MethodNode, mutable.Map[InvokeDynamicInsnNode, asm.Handle]]] =
    new ConcurrentHashMap

  def onIndyLambdaImplMethodIfPresent[T](hostClass: InternalName)(action: mutable.Map[MethodNode, mutable.Map[InvokeDynamicInsnNode, asm.Handle]] => T): Option[T] =
    indyLambdaImplMethods.get(hostClass) match {
      case null => None
      case methods => Some(methods.synchronized(action(methods)))
    }

  def onIndyLambdaImplMethod[T](hostClass: InternalName)(action: mutable.Map[MethodNode, mutable.Map[InvokeDynamicInsnNode, asm.Handle]] => T): T = {
    val methods = indyLambdaImplMethods.computeIfAbsent(hostClass, _ => mutable.Map.empty)
    methods.synchronized(action(methods))
  }

  def addIndyLambdaImplMethod(hostClass: InternalName, method: MethodNode, indy: InvokeDynamicInsnNode, handle: asm.Handle): Unit = {
    onIndyLambdaImplMethod(hostClass)(_.getOrElseUpdate(method, mutable.Map.empty)(indy) = handle)
  }

  def removeIndyLambdaImplMethod(hostClass: InternalName, method: MethodNode, indy: InvokeDynamicInsnNode): Unit = {
    onIndyLambdaImplMethodIfPresent(hostClass)(_.get(method).foreach(_.remove(indy)))
  }

  /**
   * The methods used as lambda bodies for IndyLambda instructions within `method` of `hostClass`.
   */
  def indyLambdaBodyMethods(hostClass: InternalName, method: MethodNode): Map[InvokeDynamicInsnNode, Handle] = {
    onIndyLambdaImplMethodIfPresent(hostClass)(ms => ms.getOrElse(method, Nil).toMap).getOrElse(Map.empty)
  }
  
  def isPredefLoad(insn: AbstractInsnNode): Boolean = AnalysisUtils.isModuleLoad(insn, _ == ts.PredefRef.internalName)

  // ==============================================================================================

  val primitiveAsmTypeSortToBType: Map[Int, PrimitiveBType] = Map(
    asm.Type.BOOLEAN -> BOOL,
    asm.Type.BYTE    -> BYTE,
    asm.Type.CHAR    -> CHAR,
    asm.Type.SHORT   -> SHORT,
    asm.Type.INT     -> INT,
    asm.Type.LONG    -> LONG,
    asm.Type.FLOAT   -> FLOAT,
    asm.Type.DOUBLE  -> DOUBLE
  )

  def isScalaBox(insn: MethodInsnNode): Boolean =
    insn.owner == ClassBType.scalaRuntimeBoxesRunTimeInternalName && {
      val args = asm.Type.getArgumentTypes(insn.desc)
      args.length == 1 && (primitiveAsmTypeSortToBType.get(args(0).getSort) match
        case Some(prim) => 
          val MethodNameAndType(name, tp) = ts.srBoxesRuntimeBoxToMethods(prim)
          name == insn.name && tp.descriptor == insn.desc
        case None => false)
    }

  def getScalaBox(primitiveType: asm.Type): MethodInsnNode = {
    val bType = primitiveAsmTypeSortToBType(primitiveType.getSort)
    val MethodNameAndType(name, methodBType) = ts.srBoxesRuntimeBoxToMethods(bType)
    new MethodInsnNode(Opcodes.INVOKESTATIC, ClassBType.scalaRuntimeBoxesRunTimeInternalName, name, methodBType.descriptor, /*itf =*/ false)
  }

  def getScalaUnbox(primitiveType: asm.Type): MethodInsnNode = {
    val bType = primitiveAsmTypeSortToBType(primitiveType.getSort)
    val MethodNameAndType(name, methodBType) = ts.srBoxesRuntimeUnboxToMethods(bType)
    new MethodInsnNode(Opcodes.INVOKESTATIC, ClassBType.scalaRuntimeBoxesRunTimeInternalName, name, methodBType.descriptor, /*itf =*/ false)
  }

  def isScalaUnbox(insn: MethodInsnNode): Boolean = {
    insn.owner == ClassBType.scalaRuntimeBoxesRunTimeInternalName && (primitiveAsmTypeSortToBType.get(asm.Type.getReturnType(insn.desc).getSort) match {
      case Some(prim) =>
        val MethodNameAndType(name, tp) = ts.srBoxesRuntimeUnboxToMethods(prim)
        name == insn.name && tp.descriptor == insn.desc
      case _ => false
    })
  }

  private def calleeInMap(insn: MethodInsnNode, map: Map[InternalName, MethodNameAndType]): Boolean = map.get(insn.owner) match {
    case Some(MethodNameAndType(name, tp)) => insn.name == name && insn.desc == tp.descriptor
    case _ => false
  }

  def isJavaBox(insn: MethodInsnNode): Boolean = calleeInMap(insn, ts.javaBoxMethods)
  def isJavaUnbox(insn: MethodInsnNode): Boolean = calleeInMap(insn, ts.javaUnboxMethods)

  def isPredefAutoBox(insn: MethodInsnNode): Boolean = {
    insn.owner == ts.PredefRef.internalName && (ts.predefAutoBoxMethods.get(insn.name) match {
      case Some(tp) => insn.desc == tp.descriptor
      case _ => false
    })
  }

  def isPredefAutoUnbox(insn: MethodInsnNode): Boolean = {
    insn.owner == ts.PredefRef.internalName && (ts.predefAutoUnboxMethods.get(insn.name) match {
      case Some(tp) => insn.desc == tp.descriptor
      case _ => false
    })
  }

  def getBoxedUnit: FieldInsnNode =
    new FieldInsnNode(Opcodes.GETSTATIC, ts.srBoxedUnitRef.internalName, "UNIT", ts.srBoxedUnitRef.descriptor)

  def isBoxedUnit(insn: AbstractInsnNode): Boolean = {
    insn.getOpcode == Opcodes.GETSTATIC && {
      val fi = insn.asInstanceOf[FieldInsnNode]
      fi.owner == ts.srBoxedUnitRef.internalName && fi.name == "UNIT" && fi.desc == ts.srBoxedUnitRef.descriptor
    }
  }

  def isRefCreate(insn: MethodInsnNode): Boolean = calleeInMap(insn, ts.srRefCreateMethods)
  def isRefZero(insn: MethodInsnNode): Boolean = calleeInMap(insn, ts.srRefZeroMethods)

  def isPrimitiveBoxConstructor(insn: MethodInsnNode): Boolean = calleeInMap(insn, ts.primitiveBoxConstructors)
  def isRuntimeRefConstructor(insn: MethodInsnNode): Boolean = calleeInMap(insn, ts.srRefConstructors)
  def isTupleConstructor(insn: MethodInsnNode): Boolean = calleeInMap(insn, ts.tupleClassConstructors)
  def isTupleApply(insn: MethodInsnNode): Boolean = insn.owner.startsWith("scala/Tuple") && insn.owner.endsWith("$") && insn.name == "apply"

  def runtimeRefClassBoxedType(refClass: InternalName): asm.Type = asm.Type.getArgumentTypes(ts.srRefCreateMethods(refClass).methodType.descriptor)(0)

  def isSideEffectFreeConstructorCall(insn: MethodInsnNode): Boolean = {
    insn.name == BCodeUtils.INSTANCE_CONSTRUCTOR_NAME && sideEffectFreeConstructors((insn.owner, insn.desc))
  }

  def isNewForSideEffectFreeConstructor(insn: AbstractInsnNode): Boolean = {
    insn.getOpcode == Opcodes.NEW && {
      val ti = insn.asInstanceOf[TypeInsnNode]
      classesOfSideEffectFreeConstructors.contains(ti.desc)
    }
  }

  def isSideEffectFreeCall(mi: MethodInsnNode): Boolean = {
    isScalaBox(mi) ||  // not Scala unbox, it may CCE
      isJavaBox(mi) || // not Java unbox, it may NPE
      isSideEffectFreeConstructorCall(mi) ||
      AnalysisUtils.isClassTagApply(mi)
  }

  // methods that are known to return a non-null result
  def isNonNullMethodInvocation(mi: MethodInsnNode): Boolean = {
    isJavaBox(mi) || isScalaBox(mi) || isPredefAutoBox(mi) || isRefCreate(mi) || isRefZero(mi) || AnalysisUtils.isClassTagApply(mi) ||
      isTupleApply(mi)
  }

  // unused objects created by these constructors are eliminated by pushPop
  private lazy val sideEffectFreeConstructors: Set[(String, String)] =
    val ownerDesc = (p: (InternalName, MethodNameAndType)) => (p._1, p._2.methodType.descriptor)
    ts.primitiveBoxConstructors.map(ownerDesc).toSet ++
      ts.srRefConstructors.map(ownerDesc) ++
      ts.tupleClassConstructors.map(ownerDesc) ++ Set(
      (ts.ObjectRef.internalName, MethodBType(Nil, UNIT).descriptor),
      (ts.StringRef.internalName, MethodBType(Nil, UNIT).descriptor),
      (ts.StringRef.internalName, MethodBType(List(ts.StringRef), UNIT).descriptor),
      (ts.StringRef.internalName, MethodBType(List(ArrayBType(CHAR)), UNIT).descriptor))

  lazy val modulesAllowSkipInitialization: Set[InternalName] =
    Set(
      "scala/Predef$",
      "scala/runtime/ScalaRunTime$",
      "scala/runtime/Scala3RunTime$",
      "scala/reflect/ClassTag$",
      "scala/reflect/ManifestFactory$",
      "scala/Array$",
      "scala/collection/ArrayOps$",
      "scala/collection/StringOps$",
      "scala/TupleXXL$"
    ) ++ (1 to Definitions.MaxTupleArity).map(n => s"scala/Tuple$n$$") ++ AnalysisUtils.primitiveTypes.keysIterator

  private lazy val classesOfSideEffectFreeConstructors: Set[String] =
    sideEffectFreeConstructors.map(_._1)

  private val nonForwarderInstructionTypes: BitSet = {
    import AbstractInsnNode.*
    BitSet(FIELD_INSN, INVOKE_DYNAMIC_INSN, JUMP_INSN, IINC_INSN, TABLESWITCH_INSN, LOOKUPSWITCH_INSN)
  }

  /**
   * Identify forwarders, aliases, anonfun\$adapted methods, bridges, trivial methods (x + y), etc
   * Returns
   * -1 : no match
   * 1 : trivial (no method calls), but not field getters
   * 2 : factory
   * 3 : forwarder with boxing adaptation
   * 4 : generic forwarder / alias
   *
   * TODO: should delay some checks to `canInline` (during inlining)
   * problem is: here we don't have access to the callee / accessed field, so we can't check accessibility
   *   - INVOKESPECIAL is not the only way to call private methods, INVOKESTATIC is also possible
   *   - the body of the callee can change between here (we're in inliner heuristics) and the point
   *     when we actually inline it (code may have been inlined into the callee)
   *   - methods accessing a public field could be inlined. on the other hand, methods accessing a private
   *     static field should not be inlined.
   */
  def looksLikeForwarderOrFactoryOrTrivial(method: MethodNode, owner: InternalName, allowPrivateCalls: Boolean): Int = {
    val paramTypes = Type.getArgumentTypes(method.desc)
    val numPrimitives = paramTypes.count(_.getSort < Type.ARRAY) + (if (Type.getReturnType(method.desc).getSort < Type.ARRAY) 1 else 0)

    val maxSize =
      3 + // forwardee call, return
        paramTypes.length + // param load
        numPrimitives * 2 + // box / unbox call, for example Predef.int2Integer
        paramTypes.length + 2 // some slack: +1 for each parameter, receiver, return value. allow things like casts.

    if (method.instructions.iterator.asScala.count(_.getOpcode > 0) > maxSize) return -1

    var numBoxConv = 0
    var numCallsOrNew = 0
    var callMi: MethodInsnNode | Null = null
    val it = method.instructions.iterator
    while (it.hasNext && numCallsOrNew < 2) {
      val i = it.next()
      val t = i.getType
      if (t == AbstractInsnNode.METHOD_INSN) {
        val mi = i.asInstanceOf[MethodInsnNode]
        // invokespecial has, well, special semantics that depend on the class it's being invoked in, see, e.g., https://stackoverflow.com/a/8950564
        if (!allowPrivateCalls && i.getOpcode == Opcodes.INVOKESPECIAL && mi.name != BCodeUtils.INSTANCE_CONSTRUCTOR_NAME) {
          numCallsOrNew = 2 // stop here: don't inline forwarders with a private or super call
        } else {
          if (isScalaBox(mi) || isScalaUnbox(mi) || isPredefAutoBox(mi) || isPredefAutoUnbox(mi) || isJavaBox(mi) || isJavaUnbox(mi))
            numBoxConv += 1
          else {
            numCallsOrNew += 1
            callMi = mi
          }
        }
      } else if (nonForwarderInstructionTypes(t)) {
        if (i.getOpcode == Opcodes.GETSTATIC) {
          if (!allowPrivateCalls && owner == i.asInstanceOf[FieldInsnNode].owner)
            numCallsOrNew = 2 // stop here: not forwarder or trivial
        } else {
          numCallsOrNew = 2 // stop here: not forwarder or trivial
        }
      }
    }
    if (numCallsOrNew > 1 || numBoxConv > paramTypes.length + 1) -1
    else if (numCallsOrNew == 0) if (numBoxConv == 0) 1 else 3
    else if (callMi.nn.name == BCodeUtils.INSTANCE_CONSTRUCTOR_NAME) 2 // if numCallsOrNew > 0 then callMi is nonnull
    else if (numBoxConv > 0) 3
    else 4
  }
}

object OptimizerUtils {


  /**
   * A pseudo-flag indicating if a MethodNode's unreachable code has been eliminated.
   *
   * The ASM Analyzer class does not compute any frame information for unreachable instructions.
   * Transformations that use an analyzer (including inlining) therefore require unreachable code
   * to be eliminated.
   *
   * This flag allows running dead code elimination whenever an analyzer is used. If the method
   * is already optimized, DCE can return early.
   */
  private val ACC_DCE_DONE = 0x2000000
  def isDceDone(method: MethodNode): Boolean = (method.access & ACC_DCE_DONE) != 0
  def setDceDone(method: MethodNode): Unit = method.access |= ACC_DCE_DONE
  def clearDceDone(method: MethodNode): Unit = method.access &= ~ACC_DCE_DONE

  private val LABEL_REACHABLE_STATUS = 0x1000000
  private def isLabelFlagSet(l: LabelNode1, f: Int): Boolean = (l.flags & f) != 0
  private def setLabelFlag(l: LabelNode1, f: Int): Unit = l.flags |= f
  private def clearLabelFlag(l: LabelNode1, f: Int): Unit = l.flags &= ~f
  def isLabelReachable(label: LabelNode): Boolean = isLabelFlagSet(label.asInstanceOf[LabelNode1], LABEL_REACHABLE_STATUS)
  def setLabelReachable(label: LabelNode): Unit = setLabelFlag(label.asInstanceOf[LabelNode1], LABEL_REACHABLE_STATUS)
  def clearLabelReachable(label: LabelNode): Unit = clearLabelFlag(label.asInstanceOf[LabelNode1], LABEL_REACHABLE_STATUS)

  // ===

  def methodSignature(classInternalName: InternalName, name: String, desc: String): String = {
    classInternalName + "::" + name + desc
  }

  def methodSignature(classInternalName: InternalName, method: MethodNode): String = {
    methodSignature(classInternalName, method.name, method.desc)
  }

  def siteString(owner: String, method: String): String = {
    val c = owner.replace('/', '.').replaceAll("\\$+", ".").replaceAll("\\.$", "")
    if (method.isEmpty) c
    else s"$c.$method"
  }

  // No point in trying to load SCoverage runtime counters, we shouldn't and can't inline them,
  // and warning about it is also not productive.
  def isSCoverage(classInternalName: InternalName): Boolean =
    classInternalName.startsWith("scala/runtime/coverage/")
}