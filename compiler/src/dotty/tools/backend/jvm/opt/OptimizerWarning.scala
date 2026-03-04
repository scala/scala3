package dotty.tools.backend.jvm.opt

import dotty.tools.backend.jvm.BTypes.InternalName
import dotty.tools.backend.jvm.{AsmUtils, BackendReporting}
import dotty.tools.backend.jvm.PostProcessorFrontendAccess.CompilerSettings
import dotty.tools.dotc.util.SourcePosition

import scala.tools.asm.tree.AbstractInsnNode


sealed trait OptimizerWarning {
  def emitWarning(settings: CompilerSettings): Boolean
}

object OptimizerWarning {
  // Method withFilter in RightBiasedEither requires an implicit empty value. Taking the value here
  // in scope allows for-comprehensions that desugar into withFilter calls (for example when using a
  // tuple de-constructor).
  implicit val emptyOptimizerWarning: OptimizerWarning = new OptimizerWarning {
    def emitWarning(settings: CompilerSettings): Boolean = false
  }
}

sealed trait MissingBytecodeWarning extends OptimizerWarning {
  override def toString: String = this match {
    case ClassNotFound(internalName) =>
      s"The classfile for $internalName could not be found on the compilation classpath."

    case MethodNotFound(name, descriptor, ownerInternalName, missingClass) =>
      val missingClassWarning = missingClass match {
        case None => ""
        case Some(c) => s"\nNote that class ${c.internalName} could not be found on the Scala classpath."
      }
      s"The method $name$descriptor could not be found in the class $ownerInternalName or any of its parents." + missingClassWarning

    case FieldNotFound(name, descriptor, ownerInternalName, missingClass) =>
      s"The field node $name$descriptor could not be found because the classfile $ownerInternalName cannot be found on the classpath." +
        missingClass.map(c => s" Reason:\n$c").getOrElse("")
  }

  def emitWarning(settings: CompilerSettings): Boolean = this match {
    case ClassNotFound(_) =>
      settings.optWarningNoInlineMissingBytecode

    case m@MethodNotFound(_, _, _, missing) =>
      if (m.isArrayMethod) false
      else settings.optWarningNoInlineMissingBytecode || missing.exists(_.emitWarning(settings))

    case FieldNotFound(_, _, _, missing) =>
      settings.optWarningNoInlineMissingBytecode || missing.exists(_.emitWarning(settings))
  }
}

final case class ClassNotFound(internalName: InternalName) extends MissingBytecodeWarning

final case class MethodNotFound(name: String, descriptor: String, ownerInternalNameOrArrayDescriptor: InternalName, missingClass: Option[ClassNotFound]) extends MissingBytecodeWarning {
  def isArrayMethod: Boolean = ownerInternalNameOrArrayDescriptor.charAt(0) == '['
}

final case class FieldNotFound(name: String, descriptor: String, ownerInternalName: InternalName, missingClass: Option[ClassNotFound]) extends MissingBytecodeWarning

final case class NoClassBTypeInfo(cause: MissingBytecodeWarning) extends OptimizerWarning {
  override def toString: String =
    cause.toString

  def emitWarning(settings: CompilerSettings): Boolean =
    cause.emitWarning(settings)
}

/**
 * Used in the CallGraph for nodes where an issue occurred determining the callee information.
 */
sealed trait CalleeInfoWarning extends OptimizerWarning {
  def declarationClass: InternalName

  def name: String

  def descriptor: String

  private def warningMessageSignature = BackendReporting.methodSignature(declarationClass, name, descriptor)

  override def toString: String = this match {
    case MethodInlineInfoIncomplete(_, _, _, cause) =>
      s"The inline information for $warningMessageSignature may be incomplete:\n" + cause

    case MethodInlineInfoMissing(_, _, _, cause) =>
      s"No inline information for method $warningMessageSignature could be found." +
        cause.map(" Possible reason:\n" + _).getOrElse("")

    case MethodInlineInfoError(_, _, _, cause) =>
      s"Error while computing the inline information for method $warningMessageSignature:\n" + cause
  }

  def emitWarning(settings: CompilerSettings): Boolean = this match {
    case MethodInlineInfoIncomplete(_, _, _, cause) => cause.emitWarning(settings)

    case MethodInlineInfoMissing(_, _, _, Some(cause)) => cause.emitWarning(settings)
    case MethodInlineInfoMissing(_, _, _, None) => settings.optWarningNoInlineMissingBytecode

    case MethodInlineInfoError(_, _, _, cause) => cause.emitWarning(settings)
  }
}

final case class MethodInlineInfoIncomplete(declarationClass: InternalName, name: String, descriptor: String, cause: ClassInlineInfoWarning) extends CalleeInfoWarning

final case class MethodInlineInfoMissing(declarationClass: InternalName, name: String, descriptor: String, cause: Option[ClassInlineInfoWarning]) extends CalleeInfoWarning

final case class MethodInlineInfoError(declarationClass: InternalName, name: String, descriptor: String, cause: OptimizerWarning) extends CalleeInfoWarning

sealed trait CannotInlineWarning extends OptimizerWarning {
  def calleeDeclarationClass: InternalName

  def name: String

  def descriptor: String

  /** Either the callee or the callsite is annotated @inline */
  def annotatedInline: Boolean

  private def calleeMethodSig = BackendReporting.methodSignature(calleeDeclarationClass, name, descriptor)

  override def toString: String = {
    val annotWarn = if (annotatedInline) " is annotated @inline but" else ""
    val warning = s"$calleeMethodSig$annotWarn could not be inlined:\n"
    val reason = this match {
      case CalleeNotFinal(_, _, _, _) =>
        s"The method is not final and may be overridden."
      case IllegalAccessInstructions(_, _, _, _, callsiteClass, instructions) =>
        val suffix = if (instructions.lengthCompare(1) > 0) "s" else ""
        s"The callee $calleeMethodSig contains the instruction$suffix ${instructions.map(AsmUtils.textify).mkString(", ")}" +
          s"\nthat would cause an IllegalAccessError when inlined into class $callsiteClass."

      case IllegalAccessCheckFailed(_, _, _, _, callsiteClass, instruction, cause) =>
        s"""|Failed to check if $calleeMethodSig can be safely inlined to $callsiteClass without causing an IllegalAccessError.
            |Checking failed for instruction ${AsmUtils.textify(instruction)}:
            |$cause"""

      case MethodWithHandlerCalledOnNonEmptyStack(_, _, _, _, callsiteClass, callsiteName, callsiteDesc) =>
        s"""|The operand stack at the callsite in ${BackendReporting.methodSignature(callsiteClass, callsiteName, callsiteDesc)} contains more values than the
            |arguments expected by the callee $calleeMethodSig. These values would be discarded
            |when entering an exception handler declared in the inlined method."""

      case SynchronizedMethod(_, _, _, _) =>
        s"Method $calleeMethodSig cannot be inlined because it is synchronized."

      case _: NoBytecode =>
        s"Method $calleeMethodSig cannot be inlined because it does not have any instructions, even though it is not abstract. The class may come from a signature jar file (such as a Bazel 'hjar')."

      case StrictfpMismatch(_, _, _, _, callsiteClass, callsiteName, callsiteDesc) =>
        s"""The callsite method ${BackendReporting.methodSignature(callsiteClass, callsiteName, callsiteDesc)}
           |does not have the same strictfp mode as the callee $calleeMethodSig.
       """.stripMargin

      case ResultingMethodTooLarge(_, _, _, _, callsiteClass, callsiteName, callsiteDesc) =>
        s"""The size of the callsite method ${BackendReporting.methodSignature(callsiteClass, callsiteName, callsiteDesc)}
           |would exceed the JVM method size limit after inlining $calleeMethodSig.
       """.stripMargin
    }
    warning + reason
  }

  def emitWarning(settings: CompilerSettings): Boolean = {
    settings.optWarningEmitAnyInlineFailed ||
      annotatedInline && settings.optWarningEmitAtInlineFailed
  }
}

final case class CalleeNotFinal(calleeDeclarationClass: InternalName, name: String, descriptor: String, annotatedInline: Boolean) extends CannotInlineWarning

final case class IllegalAccessInstructions(calleeDeclarationClass: InternalName, name: String, descriptor: String, annotatedInline: Boolean,
                                           callsiteClass: InternalName, instructions: List[AbstractInsnNode]) extends CannotInlineWarning

final case class IllegalAccessCheckFailed(calleeDeclarationClass: InternalName, name: String, descriptor: String, annotatedInline: Boolean,
                                          callsiteClass: InternalName, instruction: AbstractInsnNode, cause: OptimizerWarning) extends CannotInlineWarning

final case class MethodWithHandlerCalledOnNonEmptyStack(calleeDeclarationClass: InternalName, name: String, descriptor: String, annotatedInline: Boolean,
                                                        callsiteClass: InternalName, callsiteName: String, callsiteDesc: String) extends CannotInlineWarning

final case class SynchronizedMethod(calleeDeclarationClass: InternalName, name: String, descriptor: String, annotatedInline: Boolean) extends CannotInlineWarning

final case class NoBytecode(calleeDeclarationClass: InternalName, name: String, descriptor: String, annotatedInline: Boolean) extends CannotInlineWarning

final case class StrictfpMismatch(calleeDeclarationClass: InternalName, name: String, descriptor: String, annotatedInline: Boolean,
                                  callsiteClass: InternalName, callsiteName: String, callsiteDesc: String) extends CannotInlineWarning

case class ResultingMethodTooLarge(calleeDeclarationClass: InternalName, name: String, descriptor: String, annotatedInline: Boolean,
                                   callsiteClass: InternalName, callsiteName: String, callsiteDesc: String) extends CannotInlineWarning

// TODO: this should be a subtype of CannotInlineWarning
// but at the place where it's created (in findIllegalAccess) we don't have the necessary data (calleeName, calleeDescriptor).
case object UnknownInvokeDynamicInstruction extends OptimizerWarning {
  override def toString = "The callee contains an InvokeDynamic instruction with an unknown bootstrap method (not a LambdaMetaFactory)."

  def emitWarning(settings: CompilerSettings): Boolean = settings.optWarningEmitAnyInlineFailed
}

/**
 * Used in `rewriteClosureApplyInvocations` when a closure apply callsite cannot be rewritten
 * to the closure body method.
 */
sealed trait RewriteClosureApplyToClosureBodyFailed extends OptimizerWarning {
  def pos: SourcePosition

  override def emitWarning(settings: CompilerSettings): Boolean = this match {
    case RewriteClosureAccessCheckFailed(_, cause) => cause.emitWarning(settings)
    case RewriteClosureIllegalAccess(_, _) => settings.optWarningEmitAnyInlineFailed
  }

  override def toString: String = this match {
    case RewriteClosureAccessCheckFailed(_, cause) =>
      s"Failed to rewrite the closure invocation to its implementation method:\n" + cause
    case RewriteClosureIllegalAccess(_, callsiteClass) =>
      s"The closure body invocation cannot be rewritten because the target method is not accessible in class $callsiteClass."
  }
}

final case class RewriteClosureAccessCheckFailed(pos: SourcePosition, cause: OptimizerWarning) extends RewriteClosureApplyToClosureBodyFailed

final case class RewriteClosureIllegalAccess(pos: SourcePosition, callsiteClass: InternalName) extends RewriteClosureApplyToClosureBodyFailed

/**
 * Used in the InlineInfo of a ClassBType, when some issue occurred obtaining the inline information.
 */
sealed trait ClassInlineInfoWarning extends OptimizerWarning {
  override def toString: String = this match {
    case NoInlineInfoAttribute(internalName) =>
      s"The Scala classfile $internalName does not have a ScalaInlineInfo attribute."

    case ClassNotFoundWhenBuildingInlineInfoFromSymbol(missingClass) =>
      s"Failed to build the inline information: $missingClass"

    case UnknownScalaInlineInfoVersion(internalName, version) =>
      s"Cannot read ScalaInlineInfo version $version in classfile $internalName. Use a more recent compiler."
  }

  def emitWarning(settings: CompilerSettings): Boolean = this match {
    case NoInlineInfoAttribute(_) => settings.optWarningNoInlineMissingScalaInlineInfoAttr
    case ClassNotFoundWhenBuildingInlineInfoFromSymbol(cause) => cause.emitWarning(settings)
    case UnknownScalaInlineInfoVersion(_, _) => settings.optWarningNoInlineMissingScalaInlineInfoAttr
  }
}

final case class NoInlineInfoAttribute(internalName: InternalName) extends ClassInlineInfoWarning

final case class ClassNotFoundWhenBuildingInlineInfoFromSymbol(missingClass: ClassNotFound) extends ClassInlineInfoWarning

final case class UnknownScalaInlineInfoVersion(internalName: InternalName, version: Int) extends ClassInlineInfoWarning