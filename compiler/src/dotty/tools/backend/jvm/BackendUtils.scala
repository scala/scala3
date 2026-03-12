package dotty.tools
package backend.jvm

import dotty.tools.backend.jvm.BCodeUtils.FrameExtensions
import dotty.tools.backend.jvm.BTypes.InternalName
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions
import dotty.tools.backend.jvm.PostProcessorFrontendAccess.Lazy
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Definitions
import dotty.tools.dotc.core.Flags.{JavaStatic, Method}
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{Symbol, TermSymbol}
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SourcePosition

import java.util.concurrent.ConcurrentHashMap
import scala.annotation.switch
import scala.collection.{BitSet, mutable}
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import scala.tools.asm
import scala.tools.asm.Opcodes.{ANEWARRAY, NEWARRAY}
import scala.tools.asm.tree.*
import scala.tools.asm.{Handle, Opcodes, Type}

/**
 * This component hosts tools and utilities used in the backend that require access to a `CoreBTypes`
 * instance.
 */
class BackendUtils(val ppa: PostProcessorFrontendAccess, val ts: CoreBTypes)(using Context) {

  /**
   * Classes with indyLambda closure instantiations where the SAM type is serializable (e.g. Scala's
   * FunctionN) need a `\$deserializeLambda\$` method. This map contains classes for which such a
   * method has been generated. It is used during ordinary code generation, as well as during
   * inlining: when inlining an indyLambda instruction into a class, we need to make sure the class
   * has the method.
   */
  private val indyLambdaImplMethods: Lazy[ConcurrentHashMap[InternalName, mutable.Map[MethodNode, mutable.Map[InvokeDynamicInsnNode, asm.Handle]]]] =
    ppa.perRunLazy(new ConcurrentHashMap)

  lazy val classfileVersion: Int = BackendUtils.classfileVersionMap(ppa.compilerSettings.target.toInt)

  lazy val extraProc: Int = {
    import GenBCodeOps.addFlagIf
    val majorVersion: Int = classfileVersion & 0xFF
    val emitStackMapFrame = majorVersion >= 50
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
            if indy.bsm == ts.jliLambdaMetaFactoryAltMetafactoryHandle =>
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
  *   }
  * }
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

    val cw = classNode

    // Make sure to reference the ClassBTypes of all types that are used in the code generated
    // here (e.g. java/util/Map) are initialized. Initializing a ClassBType adds it to
    // `classBTypeFromInternalNameMap`. When writing the classfile, the asm ClassWriter computes
    // stack map frames and invokes the `getCommonSuperClass` method. This method expects all
    // ClassBTypes mentioned in the source code to exist in the map.

    val mv = cw.visitMethod(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$deserializeLambda$", serializedLamdaObjDesc, null, null)
    def emitLambdaDeserializeIndy(targetMethods: Seq[Handle]): Unit = {
      mv.visitVarInsn(ALOAD, 0)
      mv.visitInvokeDynamicInsn("lambdaDeserialize", serializedLamdaObjDesc, ts.jliLambdaDeserializeBootstrapHandle, targetMethods*)
    }

    val targetMethodGroupLimit = 255 - 1 - 3 // JVM limit. See MAX_MH_ARITY in CallSite.java
    val groups: Array[Array[Handle]] = implMethodsArray.grouped(targetMethodGroupLimit).toArray
    val numGroups = groups.length

    import scala.tools.asm.Label
    val initialLabels = Array.fill(numGroups - 1)(new Label())
    val terminalLabel = new Label
    def nextLabel(i: Int) = if (i == numGroups - 2) terminalLabel else initialLabels(i + 1)

    for ((label, i) <- initialLabels.iterator.zipWithIndex) {
      mv.visitTryCatchBlock(label, nextLabel(i), nextLabel(i), ts.jlIllegalArgExceptionRef.internalName)
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
    MethodBType(ts.jliSerializedLambdaRef :: Nil, ts.ObjectRef).descriptor
  }

  /**
   * Visit the class node and collect all referenced nested classes.
   */
  def collectNestedClasses(classNode: ClassNode): (Iterable[ClassBType], Iterable[ClassBType]) = {
    val c = new NestedClassesCollector[ClassBType](nestedOnly = true) {
      def declaredNestedClasses(internalName: InternalName): List[ClassBType] =
        ts.classBTypeFromInternalName(internalName).head.info.nestedClasses

      def getClassIfNested(internalName: InternalName): Option[ClassBType] = {
        val c = ts.classBTypeFromInternalName(internalName).head
        Option.when(c.isNestedClass)(c)
      }

      def raiseError(msg: String, sig: String, e: Option[Throwable]): Unit = {
        // don't crash on invalid generic signatures
      }
    }
    c.visit(classNode)
    (c.declaredInnerClasses, c.referredInnerClasses)
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
  final def addInnerClasses(jclass: asm.ClassVisitor, declaredInnerClasses: Iterable[ClassBType], refedInnerClasses: Iterable[ClassBType]): Unit = {
    // sorting ensures nested classes are listed after their enclosing class thus satisfying the Eclipse Java compiler
    val allNestedClasses = new mutable.TreeSet[ClassBType]()(using Ordering.by(_.internalName))
    allNestedClasses ++= declaredInnerClasses
    refedInnerClasses.foreach(allNestedClasses ++= _.enclosingNestedClassesChain)
    for nestedClass <- allNestedClasses
    do {
      // Extract the innerClassEntry - we know it exists, enclosingNestedClassesChain only returns nested classes.
      val Some(e) = nestedClass.innerClassAttributeEntry: @unchecked
      jclass.visitInnerClass(e.name, e.outerName, e.innerName, e.flags)
    }
  }

  def onIndyLambdaImplMethodIfPresent[T](hostClass: InternalName)(action: mutable.Map[MethodNode, mutable.Map[InvokeDynamicInsnNode, asm.Handle]] => T): Option[T] =
    indyLambdaImplMethods.get.get(hostClass) match {
      case null => None
      case methods => Some(methods.synchronized(action(methods)))
    }

  def onIndyLambdaImplMethod[T](hostClass: InternalName)(action: mutable.Map[MethodNode, mutable.Map[InvokeDynamicInsnNode, asm.Handle]] => T): T = {
    val methods = indyLambdaImplMethods.get.computeIfAbsent(hostClass, _ => mutable.Map.empty)
    methods.synchronized(action(methods))
  }

  def addIndyLambdaImplMethod(hostClass: InternalName, method: MethodNode, indy: InvokeDynamicInsnNode, handle: asm.Handle): Unit = {
    onIndyLambdaImplMethod(hostClass)(_.getOrElseUpdate(method, mutable.Map.empty)(indy) = handle)
  }

  def removeIndyLambdaImplMethod(hostClass: InternalName, method: MethodNode, indy: InvokeDynamicInsnNode): Unit = {
    onIndyLambdaImplMethodIfPresent(hostClass)(_.get(method).foreach(_.remove(indy)))
  }

  /**
   * The methods used as lambda bodies for IndyLambda instructions within `hostClass`. Note that
   * the methods are not necessarily defined within the `hostClass` (when an IndyLambda is inlined
   * into a different class).
   */
  def indyLambdaBodyMethods(hostClass: InternalName): mutable.SortedSet[Handle] = {
    object handleOrdering extends Ordering[Handle] {
      override def compare(x: Handle, y: Handle): Int = {
        if (x eq y) return 0

        val t = Ordering.Int.compare(x.getTag, y.getTag)
        if (t != 0) return t

        val i = Ordering.Boolean.compare(x.isInterface, y.isInterface)
        if (x.isInterface != y.isInterface) return i

        val o = x.getOwner.compareTo(y.getOwner)
        if (o != 0) return o

        val n = x.getName.compareTo(y.getName)
        if (n != 0) return n

        x.getDesc.compareTo(y.getDesc)
      }
    }

    given Ordering[Handle] = handleOrdering
    val res = mutable.TreeSet.empty[Handle]
    onIndyLambdaImplMethodIfPresent(hostClass)(methods => res.addAll(methods.valuesIterator.flatMap(_.valuesIterator)))
    res
  }

  /**
   * The methods used as lambda bodies for IndyLambda instructions within `method` of `hostClass`.
   */
  def indyLambdaBodyMethods(hostClass: InternalName, method: MethodNode): Map[InvokeDynamicInsnNode, Handle] = {
    onIndyLambdaImplMethodIfPresent(hostClass)(ms => ms.getOrElse(method, Nil).toMap).getOrElse(Map.empty)
  }
  
  def isPredefLoad(insn: AbstractInsnNode): Boolean = BackendUtils.isModuleLoad(insn, _ == ts.PredefRef.internalName)

  // ==============================================================================================

  def primitiveAsmTypeToBType(primitiveType: asm.Type): PrimitiveBType = (primitiveType.getSort: @switch) match {
    case asm.Type.BOOLEAN => BOOL
    case asm.Type.BYTE    => BYTE
    case asm.Type.CHAR    => CHAR
    case asm.Type.SHORT   => SHORT
    case asm.Type.INT     => INT
    case asm.Type.LONG    => LONG
    case asm.Type.FLOAT   => FLOAT
    case asm.Type.DOUBLE  => DOUBLE
    case _            => null
  }

  def isScalaBox(insn: MethodInsnNode): Boolean = {
    insn.owner == ts.srBoxesRuntimeRef.internalName && {
      val args = asm.Type.getArgumentTypes(insn.desc)
      args.length == 1 && (ts.srBoxesRuntimeBoxToMethods.get(primitiveAsmTypeToBType(args(0))) match {
        case Some(MethodNameAndType(name, tp)) => name == insn.name && tp.descriptor == insn.desc
        case _ => false
      })
    }
  }

  def getScalaBox(primitiveType: asm.Type): MethodInsnNode = {
    val bType = primitiveAsmTypeToBType(primitiveType)
    val MethodNameAndType(name, methodBType) = ts.srBoxesRuntimeBoxToMethods(bType)
    new MethodInsnNode(Opcodes.INVOKESTATIC, ts.srBoxesRuntimeRef.internalName, name, methodBType.descriptor, /*itf =*/ false)
  }

  def getScalaUnbox(primitiveType: asm.Type): MethodInsnNode = {
    val bType = primitiveAsmTypeToBType(primitiveType)
    val MethodNameAndType(name, methodBType) = ts.srBoxesRuntimeUnboxToMethods(bType)
    new MethodInsnNode(Opcodes.INVOKESTATIC, ts.srBoxesRuntimeRef.internalName, name, methodBType.descriptor, /*itf =*/ false)
  }

  def isScalaUnbox(insn: MethodInsnNode): Boolean = {
    insn.owner == ts.srBoxesRuntimeRef.internalName && (ts.srBoxesRuntimeUnboxToMethods.get(primitiveAsmTypeToBType(asm.Type.getReturnType(insn.desc))) match {
      case Some(MethodNameAndType(name, tp)) => name == insn.name && tp.descriptor == insn.desc
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
    insn.name == GenBCode.INSTANCE_CONSTRUCTOR_NAME && sideEffectFreeConstructors((insn.owner, insn.desc))
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
      BackendUtils.isClassTagApply(mi)
  }

  // methods that are known to return a non-null result
  def isNonNullMethodInvocation(mi: MethodInsnNode): Boolean = {
    isJavaBox(mi) || isScalaBox(mi) || isPredefAutoBox(mi) || isRefCreate(mi) || isRefZero(mi) || BackendUtils.isClassTagApply(mi) ||
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
    if (!ppa.compilerSettings.optAllowSkipCoreModuleInit) Set.empty
    else Set(
      "scala/Predef$",
      "scala/runtime/ScalaRunTime$",
      "scala/runtime/Scala3RunTime$",
      "scala/reflect/ClassTag$",
      "scala/reflect/ManifestFactory$",
      "scala/Array$",
      "scala/collection/ArrayOps$",
      "scala/collection/StringOps$",
      "scala/TupleXXL$"
    ) ++ (1 to Definitions.MaxTupleArity).map(n => s"scala/Tuple$n$$") ++ BackendUtils.primitiveTypes.keysIterator

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
    var callMi: MethodInsnNode = null
    val it = method.instructions.iterator
    while (it.hasNext && numCallsOrNew < 2) {
      val i = it.next()
      val t = i.getType
      if (t == AbstractInsnNode.METHOD_INSN) {
        val mi = i.asInstanceOf[MethodInsnNode]
        // invokespecial has, well, special semantics that depend on the class it's being invoked in, see, e.g., https://stackoverflow.com/a/8950564
        if (!allowPrivateCalls && i.getOpcode == Opcodes.INVOKESPECIAL && mi.name != GenBCode.INSTANCE_CONSTRUCTOR_NAME) {
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
    else if (callMi.name == GenBCode.INSTANCE_CONSTRUCTOR_NAME) 2
    else if (numBoxConv > 0) 3
    else 4
  }
}

object BackendUtils {

  private def debugLevel = 3 // 0 -> no debug info; 1-> filename; 2-> lines; 3-> varnames
  final val emitSource = debugLevel >= 1
  final val emitLines = debugLevel >= 2
  final val emitVars = debugLevel >= 3

  private lazy val primitiveTypes: Map[String, asm.Type] = Map(
    ("Unit", asm.Type.VOID_TYPE),
    ("Boolean", asm.Type.BOOLEAN_TYPE),
    ("Char", asm.Type.CHAR_TYPE),
    ("Byte", asm.Type.BYTE_TYPE),
    ("Short", asm.Type.SHORT_TYPE),
    ("Int", asm.Type.INT_TYPE),
    ("Float", asm.Type.FLOAT_TYPE),
    ("Long", asm.Type.LONG_TYPE),
    ("Double", asm.Type.DOUBLE_TYPE))

  lazy val classfileVersionMap: Map[Int, Int] = Map(
    17 -> asm.Opcodes.V17,
    18 -> asm.Opcodes.V18,
    19 -> asm.Opcodes.V19,
    20 -> asm.Opcodes.V20,
    21 -> asm.Opcodes.V21,
    22 -> asm.Opcodes.V22,
    23 -> asm.Opcodes.V23,
    24 -> asm.Opcodes.V24,
    25 -> asm.Opcodes.V25,
    26 -> asm.Opcodes.V26,
  )


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
  def isLabelReachable(label: LabelNode) = isLabelFlagSet(label.asInstanceOf[LabelNode1], LABEL_REACHABLE_STATUS)
  def setLabelReachable(label: LabelNode) = setLabelFlag(label.asInstanceOf[LabelNode1], LABEL_REACHABLE_STATUS)
  def clearLabelReachable(label: LabelNode) = clearLabelFlag(label.asInstanceOf[LabelNode1], LABEL_REACHABLE_STATUS)

  // ==============================================================================================

  def isModuleLoad(insn: AbstractInsnNode, nameMatches: InternalName => Boolean): Boolean = insn match {
    case fi: FieldInsnNode =>
      fi.getOpcode == Opcodes.GETSTATIC &&
        nameMatches(fi.owner) &&
        fi.name == "MODULE$" &&
        fi.desc.length == fi.owner.length + 2 &&
        fi.desc.regionMatches(1, fi.owner, 0, fi.owner.length)
    case _ => false
  }

  def isJavaLangStaticLoad(insn: AbstractInsnNode): Boolean = insn match {
    case fi: FieldInsnNode =>
      fi.getOpcode == Opcodes.GETSTATIC &&
      fi.owner.startsWith("java/lang/")
    case _ => false
  }

  // ==============================================================================================

  def isArrayGetLength(mi: MethodInsnNode): Boolean = mi.owner == "java/lang/reflect/Array" && mi.name == "getLength" && mi.desc == "(Ljava/lang/Object;)I"

  // If argument i of the method is null-checked, the bit `i+1` of the result is 1
  def argumentsNullCheckedByCallee(mi: MethodInsnNode): Long = {
    if (isArrayGetLength(mi)) 1
    else 0
  }

  // ==============================================================================================

  final case class LambdaMetaFactoryCall(indy: InvokeDynamicInsnNode, samMethodType: asm.Type, implMethod: Handle, instantiatedMethodType: asm.Type)

  object LambdaMetaFactoryCall {
    private val lambdaMetaFactoryMetafactoryHandle = new Handle(
      Opcodes.H_INVOKESTATIC,
      "java/lang/invoke/LambdaMetafactory",
      "metafactory",
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;",
      /* itf = */ false)

    private val lambdaMetaFactoryAltMetafactoryHandle = new Handle(
      Opcodes.H_INVOKESTATIC,
      "java/lang/invoke/LambdaMetafactory",
      "altMetafactory",
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;",
      /* itf = */ false)

    def unapply(insn: AbstractInsnNode): Option[(InvokeDynamicInsnNode, asm.Type, Handle, asm.Type, Array[asm.Type])] = insn match {
      case indy: InvokeDynamicInsnNode if indy.bsm == lambdaMetaFactoryMetafactoryHandle || indy.bsm == lambdaMetaFactoryAltMetafactoryHandle =>
        indy.bsmArgs match {
          case Array(samMethodType: asm.Type, implMethod: Handle, instantiatedMethodType: asm.Type, _*) =>
            // LambdaMetaFactory performs a number of automatic adaptations when invoking the lambda
            // implementation method (casting, boxing, unboxing, and primitive widening, see Javadoc).
            //
            // The closure optimizer supports only one of those adaptations: it will cast arguments
            // to the correct type when re-writing a closure call to the body method. Example:
            //
            //   val fun: String => String = l => l
            //   val l = List("")
            //   fun(l.head)
            //
            // The samMethodType of Function1 is `(Object)Object`, while the instantiatedMethodType
            // is `(String)String`. The return type of `List.head` is `Object`.
            //
            // The implMethod has the signature `C$anonfun(String)String`.
            //
            // At the closure callsite, we have an `INVOKEINTERFACE Function1.apply (Object)Object`,
            // so the object returned by `List.head` can be directly passed into the call (no cast).
            //
            // The closure object will cast the object to String before passing it to the implMethod.
            //
            // When re-writing the closure callsite to the implMethod, we have to insert a cast.
            //
            // The check below ensures that
            //   (1) the implMethod type has the expected arguments (captured types plus argument types
            //       from instantiatedMethodType)
            //   (2) the receiver of the implMethod matches the first captured type, if any, otherwise
            //       the first parameter type of instantiatedMethodType
            //   (3) all parameters that are not the same in samMethodType and instantiatedMethodType
            //       are reference types, so that we can insert casts to perform the same adaptation
            //       that the closure object would.

            val isStatic                   = implMethod.getTag == Opcodes.H_INVOKESTATIC
            val indyParamTypes             = asm.Type.getArgumentTypes(indy.desc)
            val instantiatedMethodArgTypes = instantiatedMethodType.getArgumentTypes

            val (receiverType, expectedImplMethodType) =
              if (isStatic) {
                val paramTypes = indyParamTypes ++ instantiatedMethodArgTypes
                (None, asm.Type.getMethodType(instantiatedMethodType.getReturnType, paramTypes*))
              } else if (implMethod.getTag == Opcodes.H_NEWINVOKESPECIAL) {
                (Some(instantiatedMethodType.getReturnType), asm.Type.getMethodType(asm.Type.VOID_TYPE, instantiatedMethodArgTypes*))
              } else {
                if (indyParamTypes.nonEmpty) {
                  val paramTypes = indyParamTypes.tail ++ instantiatedMethodArgTypes
                  (Some(indyParamTypes(0)), asm.Type.getMethodType(instantiatedMethodType.getReturnType, paramTypes*))
                } else {
                  val paramTypes = instantiatedMethodArgTypes.tail
                  (Some(instantiatedMethodArgTypes(0)), asm.Type.getMethodType(instantiatedMethodType.getReturnType, paramTypes*))
                }
              }

            val isIndyLambda =
              asm.Type.getType(implMethod.getDesc).getArgumentTypes.sameElements(expectedImplMethodType.getArgumentTypes) // (1)
                && receiverType.forall(rt => implMethod.getOwner == rt.getInternalName)  // (2)
                && samMethodType.getArgumentTypes.corresponds(instantiatedMethodArgTypes)((samArgType, instArgType) =>
                samArgType == instArgType || BCodeUtils.isReference(samArgType) && BCodeUtils.isReference(instArgType)) // (3)

            if (isIndyLambda) Some((indy, samMethodType, implMethod, instantiatedMethodType, indyParamTypes))
            else None

          case _ => None
        }
      case _ => None
    }
  }

  // ==============================================================================================

  def isRuntimeArrayLoadOrUpdate(insn: AbstractInsnNode): Boolean = insn.getOpcode == Opcodes.INVOKEVIRTUAL && {
    val mi = insn.asInstanceOf[MethodInsnNode]
    mi.owner == "scala/runtime/ScalaRunTime$" && {
      mi.name == "array_apply" && mi.desc == "(Ljava/lang/Object;I)Ljava/lang/Object;" ||
        mi.name == "array_update" && mi.desc == "(Ljava/lang/Object;ILjava/lang/Object;)V"
    }
  }

  private val primitiveManifestApplies: Map[String, String] = primitiveTypes map {
    case (k, _) => (k, s"()Lscala/reflect/ManifestFactory$$${k}Manifest;")
  }

  private def isClassTagApply(mi: MethodInsnNode): Boolean = {
    mi.owner == "scala/reflect/ClassTag$" && {
      mi.name == "apply" && mi.desc == "(Ljava/lang/Class;)Lscala/reflect/ClassTag;" ||
        primitiveManifestApplies.get(mi.name).contains(mi.desc)
    }
  }

  // ==============================================================================================

  def isTraitSuperAccessor(method: MethodNode, owner: ClassBType): Boolean = {
    owner.isInterface &&
      BCodeUtils.isSyntheticMethod(method) &&
      method.name.endsWith("$") &&
      BCodeUtils.isStaticMethod(method) &&
      BCodeUtils.findSingleCall(method, mi => mi.itf && mi.getOpcode == Opcodes.INVOKESPECIAL && mi.name + "$" == method.name).nonEmpty
  }

  def isMixinForwarder(method: MethodNode, owner: ClassBType): Boolean = {
    !owner.isInterface &&
      // isSyntheticMethod(method) && // mixin forwarders are not synthetic it seems
      !BCodeUtils.isStaticMethod(method) &&
      BCodeUtils.findSingleCall(method, mi => mi.itf && mi.getOpcode == Opcodes.INVOKESTATIC && mi.name == method.name + "$").nonEmpty
  }

  def isTraitSuperAccessorOrMixinForwarder(method: MethodNode, owner: ClassBType): Boolean = {
    isTraitSuperAccessor(method, owner) || isMixinForwarder(method, owner)
  }
  
  def traitSuperAccessorName(sym: Symbol)(using Context): String = {
    val nameString = sym.javaSimpleName
    if (sym.name == nme.TRAIT_CONSTRUCTOR) nameString
    else nameString + "$"
  }

  def makeStatifiedDefSymbol(origSym: TermSymbol, name: TermName)(using Context): TermSymbol =
    val info = origSym.info match
      case mt: MethodType =>
        MethodType(nme.SELF :: mt.paramNames, origSym.owner.typeRef :: mt.paramInfos, mt.resType)
    origSym.copy(
      name = name.toTermName,
      flags = Method | JavaStatic,
      info = info
    ).asTerm

  // ===

  def compilingArray(using Context) =
    ctx.compilationUnit.source.file.name == "Array.scala"

  private val primitiveCompilationUnits = Set(
    "Unit.scala",
    "Boolean.scala",
    "Char.scala",
    "Byte.scala",
    "Short.scala",
    "Int.scala",
    "Float.scala",
    "Long.scala",
    "Double.scala"
  )

  def compilingPrimitive(using Context) =
    primitiveCompilationUnits(ctx.compilationUnit.source.file.name)
}
