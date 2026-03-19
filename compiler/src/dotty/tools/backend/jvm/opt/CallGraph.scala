/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools
package backend.jvm
package opt

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.IntMap
import scala.collection.{concurrent, mutable}
import scala.jdk.CollectionConverters.*
import scala.tools.asm.tree.*
import scala.tools.asm.{Opcodes, Type}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.backend.jvm.BTypes.InternalName
import dotty.tools.backend.jvm.BackendReporting.*
import dotty.tools.backend.jvm.BackendUtils.LambdaMetaFactoryCall
import dotty.tools.backend.jvm.analysis.TypeFlowInterpreter.{LMFValue, ParamValue}
import dotty.tools.backend.jvm.analysis.*
import BCodeUtils.*
import dotty.tools.dotc.util.{SourcePosition, NoSourcePosition}
import dotty.tools.backend.jvm.PostProcessorFrontendAccess.Lazy

class CallGraph(frontendAccess: PostProcessorFrontendAccess,
                byteCodeRepository: BCodeRepository, bTypesFromClassfile: BTypesFromClassfile,
                inlineInfoLoader: InlineInfoLoader, ts: CoreBTypes)(using Context) {

  /**
   * The call graph contains the callsites in the program being compiled.
   *
   * Indexing the call graph by the containing MethodNode and the invocation MethodInsnNode allows
   * finding callsites efficiently. For example, an inlining heuristic might want to know all
   * callsites within a callee method.
   *
   * Note that the call graph is not guaranteed to be complete: callsites may be missing. In
   * particular, if a method is very large, all of its callsites might not be in the hash map.
   * The reason is that adding a method to the call graph requires running an ASM analyzer, which
   * can be too slow.
   *
   * Note that call graph entries (Callsite instances) keep a reference to the invocation
   * MethodInsnNode, which keeps all AbstractInsnNodes of the method reachable. Adding classes
   * from the classpath to the call graph (in addition to classes being compiled) may prevent
   * method instruction nodes from being GCd. The BCodeRepository has a fixed size cache for
   * parsed ClassNodes - keeping all ClassNodes alive consumed too much memory.
   * The call graph is less problematic because only methods being called are kept alive, not entire
   * classes. But we should keep an eye on this.
   */
  val callsites: Lazy[mutable.Map[MethodNode, Map[MethodInsnNode, Callsite]]] = frontendAccess.perRunLazy(concurrent.TrieMap.empty withDefaultValue Map.empty)

  /**
   * Closure instantiations in the program being compiled.
   *
   * Indexing closure instantiations by the containing MethodNode is beneficial for the closure
   * optimizer: finding callsites to re-write requires running a producers-consumers analysis on
   * the method. Here the closure instantiations are already grouped by method.
   */
  //currently single threaded access only
  val closureInstantiations: Lazy[mutable.Map[MethodNode, Map[InvokeDynamicInsnNode, ClosureInstantiation]]] = frontendAccess.perRunLazy(concurrent.TrieMap.empty withDefaultValue Map.empty)

  /**
   * Store the position of every MethodInsnNode during code generation. This allows each callsite
   * in the call graph to remember its source position, which is required for inliner warnings.
   */
  val callsitePositions: Lazy[concurrent.Map[MethodInsnNode, SourcePosition]] = frontendAccess.perRunLazy(TrieMap.empty)

  /**
   * Stores callsite instructions of invocations annotated `f(): @inline/noinline`.
   * Instructions are added during code generation (BCodeBodyBuilder). The maps are then queried
   * when building the CallGraph, every Callsite object has an annotated(No)Inline field.
   */
  //currently single threaded access only
  private val inlineAnnotatedCallsites: Lazy[mutable.Set[MethodInsnNode]] = frontendAccess.perRunLazy(mutable.Set.empty)
  //currently single threaded access only
  private val noInlineAnnotatedCallsites: Lazy[mutable.Set[MethodInsnNode]] = frontendAccess.perRunLazy(mutable.Set.empty)

  // Contains `INVOKESPECIAL` instructions that were cloned by the inliner and need to be resolved
  // statically by the call graph. See Inliner.maybeInlinedLater.
  val staticallyResolvedInvokespecial: Lazy[mutable.Set[MethodInsnNode]] = frontendAccess.perRunLazy(mutable.Set.empty)

  def isStaticCallsite(call: MethodInsnNode): Boolean = {
    val opc = call.getOpcode
    opc == Opcodes.INVOKESTATIC || opc == Opcodes.INVOKESPECIAL && staticallyResolvedInvokespecial.get(call)
  }

  def removeCallsite(invocation: MethodInsnNode, methodNode: MethodNode): Option[Callsite] = {
    val methodCallsites = callsites.get(methodNode)
    val newCallsites = methodCallsites - invocation
    if (newCallsites.isEmpty) callsites.get.subtractOne(methodNode)
    else callsites.get(methodNode) = newCallsites
    methodCallsites.get(invocation)
  }

  def addCallsite(callsite: Callsite): Unit = {
    val methodCallsites = callsites.get(callsite.callsiteMethod)
    callsites.get(callsite.callsiteMethod) = methodCallsites + (callsite.callsiteInstruction -> callsite)
  }

  def containsCallsite(callsite: Callsite): Boolean = callsites.get(callsite.callsiteMethod).contains(callsite.callsiteInstruction)

  def removeClosureInstantiation(indy: InvokeDynamicInsnNode, methodNode: MethodNode): Option[ClosureInstantiation] = {
    val methodClosureInits = closureInstantiations.get(methodNode)
    val newClosureInits = methodClosureInits - indy
    if (newClosureInits.isEmpty) closureInstantiations.get.subtractOne(methodNode)
    else closureInstantiations.get(methodNode) = newClosureInits
    methodClosureInits.get(indy)
  }

  def addClass(classNode: ClassNode): Unit = {
    val classType = bTypesFromClassfile.classBTypeFromClassNode(classNode, None)
    classType.foreach(ct => classNode.methods.asScala.foreach(addMethod(_, ct)))
  }

  def refresh(methodNode: MethodNode, definingClass: ClassBType): Unit = {
    callsites.get.subtractOne(methodNode)
    closureInstantiations.get.subtractOne(methodNode)
    // callsitePositions, inlineAnnotatedCallsites, noInlineAnnotatedCallsites, staticallyResolvedInvokespecial
    // are left unchanged. They contain individual instructions, the state for those remains valid in case
    // the inliner performs a rollback.
    addMethod(methodNode, definingClass)
  }

  def addMethod(methodNode: MethodNode, definingClass: ClassBType): Unit = {
    if (!BCodeUtils.isAbstractMethod(methodNode) && !BCodeUtils.isNativeMethod(methodNode) && Limits.sizeOKForBasicValue(methodNode)) {
      lazy val typeAnalyzer = new NonLubbingTypeFlowAnalyzer(methodNode, definingClass.internalName)

      var methodCallsites = Map.empty[MethodInsnNode, Callsite]
      var methodClosureInstantiations = Map.empty[InvokeDynamicInsnNode, ClosureInstantiation]

      methodNode.instructions.iterator.asScala foreach {
        case call: MethodInsnNode if typeAnalyzer.frameAt(call) != null => // skips over unreachable code
          // JVMS 6.5 invokespecial: " If all of the following are true, let C be the direct superclass of the current class"
          def isSuperCall: Boolean =
            call.getOpcode == Opcodes.INVOKESPECIAL &&
              call.name != GenBCode.INSTANCE_CONSTRUCTOR_NAME && {
              val owner = call.owner
              definingClass.internalName != owner && {
                var nextSuper = definingClass.info.superClass
                while (nextSuper.nonEmpty) {
                  if (nextSuper.get.internalName == owner) return true
                  nextSuper = nextSuper.get.info.superClass
                }
                false
              }
            }

          val paramTps = Type.getArgumentTypes(call.desc)
          // This is the type where method lookup starts (implemented in byteCodeRepository.methodNode)
          val preciseOwner =
            if (isStaticCallsite(call)) call.owner
            else if (isSuperCall) definingClass.info.superClass.get.internalName
            else if (call.getOpcode == Opcodes.INVOKESPECIAL) call.owner
            else {
              // invokevirtual, invokeinterface: start search at the type of the receiver
              val f = typeAnalyzer.frameAt(call)
              // Not Type.getArgumentsAndReturnSizes: in asm.Frame, size-2 values use a single stack slot
              val numParams = paramTps.length
              val peekedType = f.peekStack(numParams).getType
              // This check feels wrong, and the fallback is dubious, but what else can we do here?
              if f.peekStack(numParams).getType == null then call.owner
              else peekedType.getInternalName
            }

          val callee: Either[OptimizerWarning, Callee] = {
            for {
              (method, declarationClass) <- byteCodeRepository.methodNode(preciseOwner, call.name, call.desc)
              ((declarationClassNode, declarationModuleNode), calleeSourceFilePath) <- byteCodeRepository.classNodeAndSourceFilePath(declarationClass)
              declarationClassBType <- bTypesFromClassfile.classBTypeFromClassNode(declarationClassNode, declarationModuleNode)
            } yield {
              val info = analyzeCallsite(method, declarationClassBType, call, paramTps, calleeSourceFilePath, definingClass)
              import info._
              Callee(
                callee = method,
                calleeDeclarationClass = declarationClassBType,
                isStaticallyResolved = isStaticallyResolved,
                sourceFilePath = sourceFilePath,
                annotatedInline = annotatedInline,
                annotatedNoInline = annotatedNoInline,
                samParamTypes = info.samParamTypes,
                calleeInfoWarning = warning)
            }
          }

          val argInfos = computeArgInfos(callee, call, paramTps, typeAnalyzer)

          // A nullness analysis could be used to prevent emitting unnecessary receiver null checks
          // when inlining non-static callsites. However, LocalOpt's nullness cleanup will also do
          // it after the fact, so we can avoid running the nullness analysis when building the call
          // graph (or when inlining).
          val receiverNotNull = call.getOpcode == Opcodes.INVOKESTATIC

          val pos = callsitePositions.get.getOrElse(call, NoSourcePosition)
          methodCallsites += call -> callee.fold(
            w => UnknownCallsite(
              callsiteInstruction = call,
              callsiteMethod = methodNode,
              callsiteClass = definingClass,
              argInfos = argInfos,
              callsitePosition = pos,
              warning = w
            ),
            c => KnownCallsite(
                   callsiteInstruction = call,
                   callsiteMethod = methodNode,
                   callsiteClass = definingClass,
                   callee = c,
                   argInfos = argInfos,
                   callsiteStackHeight = typeAnalyzer.frameAt(call).getStackSize,
                   receiverKnownNotNull = receiverNotNull,
                   callsitePosition = pos,
                   annotatedInline = inlineAnnotatedCallsites.get(call),
                   annotatedNoInline = noInlineAnnotatedCallsites.get(call)
                 )
          )

        case LambdaMetaFactoryCall(indy, samMethodType, implMethod, instantiatedMethodType, indyParamTypes) if typeAnalyzer.frameAt(indy) != null =>
          val lmf = LambdaMetaFactoryCall(indy, samMethodType, implMethod, instantiatedMethodType)
          computeCapturedArgInfos(lmf, indyParamTypes, typeAnalyzer).foreach(capturedArgInfos =>
            methodClosureInstantiations += indy -> ClosureInstantiation(
              lmf,
              methodNode,
              definingClass,
              capturedArgInfos)
          )

        case _ =>
      }

      callsites.get(methodNode) = methodCallsites
      closureInstantiations.get(methodNode) = methodClosureInstantiations
    }
  }


  /**
   * Clone the instructions in `methodNode` into a new [[InsnList]], mapping labels according to
   * the `labelMap`.
   *
   * For invocation instructions, set the callGraph.callsitePositions to the `callsitePos`.
   *
   * Returns
   *   - the new instruction list
   *   - a map from old to new instructions
   *   - a bit set containing local variable indices that are stored into
   */
  def cloneInstructions(methodNode: MethodNode, labelMap: Map[LabelNode, LabelNode], callsitePos: SourcePosition, keepLineNumbers: Boolean): (InsnList, Map[AbstractInsnNode, AbstractInsnNode], mutable.BitSet) = {
    val javaLabelMap = labelMap.asJava
    val result = new InsnList
    var map = Map.empty[AbstractInsnNode, AbstractInsnNode]
    val writtenLocals = mutable.BitSet.empty
    for (ins <- methodNode.instructions.iterator.asScala) {
      if (keepLineNumbers || ins.getType != AbstractInsnNode.LINE) {
        val cloned = ins.clone(javaLabelMap)
        if (ins.getType == AbstractInsnNode.METHOD_INSN) {
          val mi = ins.asInstanceOf[MethodInsnNode]
          val clonedMi = cloned.asInstanceOf[MethodInsnNode]
          callsitePositions.get(clonedMi) = callsitePos
          if (inlineAnnotatedCallsites.get(mi))
            inlineAnnotatedCallsites.get += clonedMi
          if (noInlineAnnotatedCallsites.get(mi))
            noInlineAnnotatedCallsites.get += clonedMi
          if (staticallyResolvedInvokespecial.get(mi))
            staticallyResolvedInvokespecial.get += clonedMi
        } else if (BCodeUtils.isStore(ins)) {
          val vi = ins.asInstanceOf[VarInsnNode]
          writtenLocals += vi.`var`
        }
        result.add(cloned)
        map += ((ins, cloned))
      }
    }
    (result, map, writtenLocals)
  }

  private def computeArgInfos(callee: Either[OptimizerWarning, Callee], callsiteInsn: MethodInsnNode, paramTps: Array[Type], typeAnalyzer: NonLubbingTypeFlowAnalyzer): IntMap[ArgInfo] = {
    callee match
      case Left(_) => IntMap.empty
      case Right(c) =>
        val numArgs = paramTps.length + (if (callsiteInsn.getOpcode == Opcodes.INVOKESTATIC) 0 else 1)
        argInfosForSams(c.samParamTypes, callsiteInsn, numArgs, typeAnalyzer)
  }

  private def computeCapturedArgInfos(lmf: LambdaMetaFactoryCall, indyParamTypes: Array[Type], typeAnalyzer: NonLubbingTypeFlowAnalyzer): Option[IntMap[ArgInfo]] = {
    var i = 0
    val capturedTypes = new Array[BType](indyParamTypes.length)
    while i < indyParamTypes.length do
      bTypesFromClassfile.bTypeForDescriptorFromClassfile(indyParamTypes(i).getDescriptor) match
        case Left(l) => return None
        case Right(bt) => capturedTypes(i) = bt
      i += 1

    val capturedSams = samTypes(capturedTypes)
    Some(argInfosForSams(capturedSams, lmf.indy, indyParamTypes.length, typeAnalyzer))
  }

  private def argInfosForSams(sams: IntMap[ClassBType], consumerInsn: AbstractInsnNode, numConsumed: Int, typeAnalyzer: NonLubbingTypeFlowAnalyzer): IntMap[ArgInfo] = {
    lazy val consumerFrame = typeAnalyzer.frameAt(consumerInsn)
    lazy val firstConsumedSlot = consumerFrame.stackTop - numConsumed + 1
    val samInfos: IntMap[ArgInfo] = sams flatMap {
      case (index, _) =>
        val argInfo = consumerFrame.getValue(firstConsumedSlot + index) match {
          case _: LMFValue => Some(FunctionLiteral)
          case p: ParamValue => Some(ForwardedParam(p.local))
          case _ => None
        }
        argInfo.map((index, _))
    }
    val isArrayLoadOrUpdateOnKnownArray = BackendUtils.isRuntimeArrayLoadOrUpdate(consumerInsn) &&
      consumerFrame.getValue(firstConsumedSlot + 1).getType.getSort == Type.ARRAY
    if (isArrayLoadOrUpdateOnKnownArray) samInfos.updated(1, StaticallyKnownArray)
    else samInfos
  }

  def samParamTypes(methodNode: MethodNode, paramTps: Array[Type], receiverType: ClassBType): Either[OptimizerWarning, IntMap[ClassBType]] = {
    var i = 0
    val paramTypes0 = new Array[BType](paramTps.length)
    while i < paramTps.length do
      bTypesFromClassfile.bTypeForDescriptorFromClassfile(paramTps(i).getDescriptor) match {
        case Left(l) => return Left(l)
        case Right(bt) => paramTypes0(i) = bt
      }
      i += 1

    val paramTypes = {
      val isStatic = BCodeUtils.isStaticMethod(methodNode)
      if (isStatic) paramTypes0 else receiverType +: paramTypes0
    }
    Right(samTypes(paramTypes))
  }

  private def samTypes(types: Array[BType]): IntMap[ClassBType] = {
    var res = IntMap.empty[ClassBType]
    for (i <- types.indices) {
      types(i) match {
        case c: ClassBType =>
          if (inlineInfoLoader.load(c.info).sam.isDefined) res = res.updated(i, c)

        case _ =>
      }
    }
    res
  }

  /**
   * Just a named tuple used as return type of `analyzeCallsite`.
   */
  private case class CallsiteInfo(
                                   isStaticallyResolved: Boolean = false,
                                   sourceFilePath: Option[String] = None,
                                   annotatedInline: Boolean = false,
                                   annotatedNoInline: Boolean = false,
                                   samParamTypes: IntMap[ClassBType] = IntMap.empty,
                                   warning: Option[CalleeInfoWarning],
                                 )

  /**
   * Analyze a callsite and gather meta-data that can be used for inlining decisions.
   */
  private def analyzeCallsite(calleeMethodNode: MethodNode, calleeDeclarationClassBType: ClassBType, call: MethodInsnNode, paramTps: Array[Type], calleeSourceFilePath: Option[String], callsiteClass: ClassBType): CallsiteInfo = {
    val methodSignature = (calleeMethodNode.name, calleeMethodNode.desc)

    // The inlineInfo.methodInfos of a ClassBType holds an InlineInfo for each method *declared*
    // within a class (not for inherited methods). Since we already have the  classBType of the
    // callee, we only check there for the methodInlineInfo, we should find it there.
    inlineInfoLoader.load(calleeDeclarationClassBType.info).methodInfos.get(methodSignature) match {
      case Some(methodInlineInfo) =>
        val owner = if call.owner.startsWith("[") then "java/lang/Object" else call.owner
        bTypesFromClassfile.classBTypeFromParsedClassfile(owner) match
          case Left(w) => CallsiteInfo(warning = Some(MethodInlineInfoError(call.owner, calleeMethodNode.name, calleeMethodNode.desc, w)))
          case Right(receiverType) =>
            // (1) Special case for trait super accessors. trait T { def f = 1 } generates a static
            // method t$ which calls `invokespecial T.f`. Even if `f` is not final, this call will
            // always resolve to `T.f`. This is a (very) special case. Otherwise, `invokespecial`
            // is only used for private methods, constructors and super calls.
            //
            // (2) A non-final method can be safe to inline if the receiver type is a final subclass. Example:
            //   class A { @inline def f = 1 }; object B extends A; B.f  // can be inlined
            //
            // TODO: (2) doesn't cover the following example:
            //   trait TravLike { def map = ... }
            //   sealed trait List extends TravLike { ... } // assume map is not overridden
            //   final case class :: / final case object Nil
            //   (l: List).map // can be inlined
            // we need to know that
            //   - the receiver is sealed
            //   - what are the children of the receiver
            //   - all children are final
            //   - none of the children overrides map
            //
            // TODO: type analysis can render more calls statically resolved. Example:
            //   new A.f  // can be inlined, the receiver type is known to be exactly A.
            val isStaticallyResolved: Boolean = {
              isStaticCallsite(call) ||
                (call.getOpcode == Opcodes.INVOKESPECIAL && receiverType == callsiteClass) || // (1)
                methodInlineInfo.effectivelyFinal ||
                inlineInfoLoader.load(receiverType.info).isEffectivelyFinal // (2)
            }

            val warning = inlineInfoLoader.load(calleeDeclarationClassBType.info).warning.map(
              MethodInlineInfoIncomplete(calleeDeclarationClassBType.internalName, calleeMethodNode.name, calleeMethodNode.desc, _))

            samParamTypes(calleeMethodNode, paramTps, receiverType) match
              case Left(w) => CallsiteInfo(warning = Some(MethodInlineInfoError(call.owner, calleeMethodNode.name, calleeMethodNode.desc, w)))
              case Right(spts) =>
                CallsiteInfo(
                  isStaticallyResolved = isStaticallyResolved,
                  sourceFilePath = calleeSourceFilePath,
                  annotatedInline = methodInlineInfo.annotatedInline,
                  annotatedNoInline = methodInlineInfo.annotatedNoInline,
                  samParamTypes = spts,
                  warning = warning)

      case None =>
        val warning = MethodInlineInfoMissing(calleeDeclarationClassBType.internalName, calleeMethodNode.name, calleeMethodNode.desc,
                                              inlineInfoLoader.load(calleeDeclarationClassBType.info).warning)
        CallsiteInfo(warning = Some(warning))
    }
  }
}

sealed trait Callsite:
  val callsiteInstruction: MethodInsnNode
  val callsiteMethod: MethodNode
  val callsiteClass: ClassBType
  val callsitePosition: SourcePosition
  val argInfos: IntMap[ArgInfo]

/**
   * A callsite in the call graph.
   *
   * @param callsiteInstruction The invocation instruction
   * @param callsiteMethod      The method containing the callsite
   * @param callsiteClass       The class containing the callsite
   * @param callee              The callee, as it appears in the invocation instruction. For virtual
   *                            calls, an override of the callee might be invoked. Also, the callee
   *                            can be abstract. Contains a warning message if the callee MethodNode
   *                            cannot be found in the bytecode repository.
   * @param argInfos            Information about the invocation receiver and arguments
   * @param callsiteStackHeight The stack height at the callsite, required by the inliner
   * @param callsitePosition    The source position of the callsite, used for inliner warnings.
   */
final case class KnownCallsite(callsiteInstruction: MethodInsnNode, callsiteMethod: MethodNode, callsiteClass: ClassBType,
                               callee: Callee, argInfos: IntMap[ArgInfo],
                               callsiteStackHeight: Int, receiverKnownNotNull: Boolean, callsitePosition: SourcePosition,
                               annotatedInline: Boolean, annotatedNoInline: Boolean) extends Callsite {
  // an annotation at the callsite takes precedence over an annotation at the definition site
  def isInlineAnnotated: Boolean = annotatedInline || (callee.annotatedInline && !annotatedNoInline)
  def isNoInlineAnnotated: Boolean = annotatedNoInline || (callee.annotatedNoInline && !annotatedInline)

  override def toString: String =
    "Invocation of" +
      s" ${callee.calleeDeclarationClass.internalName}.${callsiteInstruction.name + callsiteInstruction.desc}" +
      s"@${callsiteMethod.instructions.indexOf(callsiteInstruction)}" +
      s" in ${callsiteClass.internalName}.${callsiteMethod.name}${callsiteMethod.desc}"
}

final case class UnknownCallsite(callsiteInstruction: MethodInsnNode, callsiteMethod: MethodNode, callsiteClass: ClassBType,
                                 callsitePosition: SourcePosition, argInfos: IntMap[ArgInfo],
                                 warning: OptimizerWarning) extends Callsite

/**
 * Information about invocation arguments, obtained through data flow analysis of the callsite method.
 */
sealed trait ArgInfo
case object FunctionLiteral extends ArgInfo
final case class ForwardedParam(index: Int) extends ArgInfo
case object StaticallyKnownArray extends ArgInfo
//  final case class ArgTypeInfo(argType: BType, isPrecise: Boolean, knownNotNull: Boolean) extends ArgInfo
// can be extended, e.g., with constant types

/**
 * A callee in the call graph.
 *
 * @param callee                 The callee, as it appears in the invocation instruction. For
 *                               virtual calls, an override of the callee might be invoked. Also,
 *                               the callee can be abstract.
 * @param calleeDeclarationClass The class in which the callee is declared
 * @param isStaticallyResolved   True if the callee cannot be overridden
 * @param annotatedInline        True if the callee is annotated @inline
 * @param annotatedNoInline      True if the callee is annotated @noinline
 * @param samParamTypes          A map from parameter positions to SAM parameter types
 * @param calleeInfoWarning      An inliner warning if some information was not available while
 *                               gathering the information about this callee.
 */
final case class Callee(callee: MethodNode, calleeDeclarationClass: ClassBType,
                        isStaticallyResolved: Boolean, sourceFilePath: Option[String],
                        annotatedInline: Boolean, annotatedNoInline: Boolean,
                        samParamTypes: IntMap[ClassBType],
                        calleeInfoWarning: Option[CalleeInfoWarning]) {
  override def toString = s"Callee($calleeDeclarationClass.${callee.name})"

  def isAbstract: Boolean = isAbstractMethod(callee)
  def isSpecialMethod: Boolean = isConstructor(callee) || isNativeMethod(callee) || hasCallerSensitiveAnnotation(callee)
}

/**
 * Metadata about a closure instantiation, stored in the call graph
 *
 * @param lambdaMetaFactoryCall the InvokeDynamic instruction
 * @param ownerMethod           the method where the closure is allocated
 * @param ownerClass            the class containing the above method
 * @param capturedArgInfos      information about captured arguments. Used for updating the call
 *                              graph when re-writing a closure invocation to the body method.
 */
final case class ClosureInstantiation(lambdaMetaFactoryCall: LambdaMetaFactoryCall, ownerMethod: MethodNode, ownerClass: ClassBType, capturedArgInfos: IntMap[ArgInfo]) {
  override def toString = s"ClosureInstantiation($lambdaMetaFactoryCall, ${ownerMethod.name + ownerMethod.desc}, $ownerClass)"
}
