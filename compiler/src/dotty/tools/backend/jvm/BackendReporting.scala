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

import scala.tools.asm.tree.{AbstractInsnNode, MethodNode}
import dotty.tools.backend.jvm.BTypes.InternalName
import dotty.tools.backend.jvm.PostProcessorFrontendAccess.CompilerSettings
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.Message

import scala.util.control.ControlThrowable
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition, SrcPos}
import dotty.tools.io.FileWriters.{BufferingReporter, Report}


sealed trait BackendReporting {
  def error(message: Context ?=> Message, position: SourcePosition): Unit

  def warning(message: Context ?=> Message, position: SourcePosition): Unit

  def optimizerWarning(message: Context ?=> Message, site: String, position: SourcePosition): Unit

  def log(message: String): Unit

  def error(message: Context ?=> Message): Unit = error(message, NoSourcePosition)

  def warning(message: Context ?=> Message): Unit = warning(message, NoSourcePosition)

  def siteString(owner: String, method: String): String = {
    val c = owner.replace('/', '.').replaceAll("\\$+", ".").replaceAll("\\.$", "")
    if (method.isEmpty) c
    else s"$c.$method"
  }

  def relay(r: Report)(using ctx: Context): Unit = r match {
    case Report.Error(m, pos) => error(m(ctx), pos)
    case Report.Warning(m, pos) => warning(m(ctx), pos)
    case Report.OptimizerWarning(m, s, pos) => warning(m(ctx), pos) // TODO use the site
    case Report.Log(m) => log(m)
  }

  /** Should only be called from main compiler thread. */
  def relayReports(fromReporting: BufferingReporter)(using Context): Unit =
    val reports = fromReporting.resetReports()
    if reports.nonEmpty then
      reports.reverse.foreach(relay)
}

final class BufferingBackendReporting(using Context) extends BackendReporting {
  // We optimize access to the buffered reports for the common case - that there are no warning/errors to report
  // We could use a listBuffer etc. - but that would be extra allocation in the common case
  // Note - all access is externally synchronized, as this allow the reports to be generated in on thread and
  // consumed in another
  private var bufferedReports = List.empty[Report]

  def error(message: Context ?=> Message, position: SourcePosition): Unit = synchronized:
    bufferedReports ::= Report.Error({case given Context => message}, position)

  def warning(message: Context ?=> Message, position: SourcePosition): Unit = synchronized:
    bufferedReports ::= Report.Warning({case given Context => message}, position)

  def optimizerWarning(message: Context ?=> Message, site: String, position: SourcePosition): Unit = synchronized:
    bufferedReports ::= Report.OptimizerWarning({case given Context => message}, site, position)

  def log(message: String): Unit = synchronized:
    bufferedReports ::= Report.Log(message)

  def relayReports(toReporting: BackendReporting): Unit = synchronized:
    if bufferedReports.nonEmpty then
      bufferedReports.reverse.foreach(toReporting.relay)
      bufferedReports = Nil
}

final class DirectBackendReporting(ppa: PostProcessorFrontendAccess)(using Context) extends BackendReporting {
  def error(message: Context ?=> Message, position: SourcePosition): Unit = ppa.frontendSynch(report.error(message, position))
  def warning(message: Context ?=> Message, position: SourcePosition): Unit = ppa.frontendSynch(report.warning(message, position))
  def optimizerWarning(message: Context ?=> Message, site: String, position: SourcePosition): Unit = ppa.frontendSynch(report.warning(message, position, site))
  def log(message: String): Unit = ppa.frontendSynch(report.log(message))
}

/**
 * Utilities for error reporting.
 *
 * Defines some utility methods to make error reporting with Either easier.
 */
object BackendReporting {
  def methodSignature(classInternalName: InternalName, name: String, desc: String) = {
    classInternalName + "::" + name + desc
  }

  def methodSignature(classInternalName: InternalName, method: MethodNode): String = {
    methodSignature(classInternalName, method.name, method.desc)
  }

  def assertionError(message: String): Nothing = throw new AssertionError(message)

  implicit class RightBiasedEither[A, B](val v: Either[A, B]) extends AnyVal {
    def withFilter(f: B => Boolean)(implicit empty: A): Either[A, B] = v.filterOrElse(f, empty)

    /** Get the value, fail with an assertion if this is an error. */
    def get: B = v.fold(a => assertionError(s"$a"), identity)

    /**
     * Get the right value of an `Either` by throwing a potential error message. Can simplify the
     * implementation of methods that act on multiple `Either` instances. Instead of flat-mapping,
     * the first error can be collected as
     *
     *     tryEither {
     *       eitherOne.orThrow .... eitherTwo.orThrow ... eitherThree.orThrow
     *     }
     */
    def orThrow: B = v.fold(a => throw Invalid(a), identity)
  }

  case class Invalid[A](e: A) extends ControlThrowable

  /**
   * See documentation of orThrow above.
   */
  def tryEither[A, B](op: => Either[A, B]): Either[A, B] = try { op } catch { case Invalid(e) => Left(e.asInstanceOf[A]) }

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

  // TODO will be implemented along with optimizer -- depends on new compiler settings, etc.
  sealed trait NoClassBTypeInfo extends OptimizerWarning {
    def emitWarning(settings: CompilerSettings): Boolean = false 
  }
}
