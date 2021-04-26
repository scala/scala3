package dotty.tools
package dotc
package transform
package init

import ast.tpd._

import core._
import Decorators._, printing.SyntaxHighlighting
import Types._, Symbols._, Contexts._
import util.{ SimpleIdentityMap, SourcePosition, NoSourcePosition }

import reporting.MessageRendering
import printing.Highlighting

import Effects._, Potentials._

object Errors {
  type Errors = List[Error]
  val empty: Errors = Nil

  def show(errs: Errors)(using Context): String =
    errs.map(_.show).mkString(", ")

  sealed trait Error {
    def source: Tree
    def trace: Seq[Tree]
    def show(using Context): String

    def traceSuppressed: Boolean = false

    def issue(using Context): Unit =
      report.warning(show + stacktrace, source.srcPos)

    def toErrors: Errors = this :: Nil

    /** pinpoints in stacktrace */
    private var pinpoints: SimpleIdentityMap[Tree, String] = SimpleIdentityMap.empty

    def pinpoint(tree: Tree, msg: String): this.type =
      this.pinpoints = this.pinpoints.updated(tree, msg)
      this

    private def stacktracePrefix: String =
      val note = if traceSuppressed then " (suppressed)" else ""
      " Calling trace" +  note + ":\n"

    private val render = new MessageRendering {}

    private def pinpointText(pos: SourcePosition, msg: String, offset: Int)(using Context): String =
      val carets = render.hl("Warning") {
        if (pos.startLine == pos.endLine)
          "^" * math.max(1, pos.endColumn - pos.startColumn)
        else "^"
      }

      val padding = pos.startColumnPadding + (" " * offset)
      val marker = padding + carets
      val textline = padding + msg
      marker + "\n" + textline + "\n"

    def stacktrace(using Context): String = if (trace.isEmpty) "" else stacktracePrefix + {
      var indentCount = 0
      var last: SourcePosition = NoSourcePosition
      val sb = new StringBuilder
      trace.foreach { tree =>
        val pos = tree.sourcePos
        var pinpoint = ""
        val line =
          if pos.source.exists then
            val locText = "[ " + pos.source.file.name + ":" + (pos.line + 1) + " ]"
            val loc = Highlighting.Blue(locText)
            val code = SyntaxHighlighting.highlight(pos.lineContent)

            var prefix = loc + " "
            if locText.size <= indentCount then
              prefix = prefix +  (" " * (indentCount - locText.size + 1))
              indentCount = indentCount + 1
            else
              indentCount = locText.length + 1

            if pinpoints.contains(tree) then
              pinpoint = pinpointText(pos, pinpoints(tree), indentCount + 4)

            i"$prefix-> $code"
          else
            indentCount += 1
            val prefix = " " * indentCount
            i"$prefix-> ${tree.show}"

        if (last.source != pos.source || last.line != pos.line)
          sb.append(line + pinpoint)

        last = pos
      }
      sb.toString
    }

    /** Flatten UnsafePromotion errors
     */
    def flatten: Errors = this match {
      case unsafe: UnsafePromotion => unsafe.errors.flatMap(_.flatten)
      case _ => this :: Nil
    }
  }

  /** Access non-initialized field */
  case class AccessNonInit(field: Symbol, trace: Seq[Tree]) extends Error {
    def source: Tree = trace.last
    def show(using Context): String =
      "Access non-initialized " + field.show + "."

    override def issue(using Context): Unit =
      report.warning(show + stacktrace, field.srcPos)
  }

  case class CyclicObjectInit(objs: Seq[Symbol], trace: Seq[Tree], override val traceSuppressed: Boolean) extends Error {
    def source: Tree = trace.last
    def show(using Context): String =
      "Cyclic object initialization for " + objs.map(_.show).mkString(", ") + "."

    override def issue(using Context): Unit =
      report.warning(show + stacktrace, objs.head.srcPos)
  }

  case class ObjectLeakDuringInit(obj: Symbol, trace: Seq[Tree], override val traceSuppressed: Boolean) extends Error {
    def source: Tree = trace.last
    def show(using Context): String = obj.show + " leaked during its initialization " + "."

    override def issue(using Context): Unit =
      report.warning(show + stacktrace, obj.srcPos)
  }

  /** Promote `this` under initialization to fully-initialized */
  case class PromoteThis(pot: ThisRef, source: Tree, trace: Seq[Tree]) extends Error {
    def show(using Context): String = "Promote the value under initialization to fully-initialized."
  }

  /** Promote `this` under initialization to fully-initialized */
  case class PromoteWarm(pot: Warm, source: Tree, trace: Seq[Tree]) extends Error {
    def show(using Context): String =
      "Promoting the value under initialization to fully-initialized."
  }

  /** Promote a cold value under initialization to fully-initialized */
  case class PromoteCold(source: Tree, trace: Seq[Tree]) extends Error {
    def show(using Context): String =
      "Promoting the value " + source.show + " to fully-initialized while it is under initialization" + "."
  }

  case class AccessCold(field: Symbol, source: Tree, trace: Seq[Tree]) extends Error {
    def show(using Context): String =
      "Access field " + source.show + " on a value with an unknown initialization status" + "."
  }

  case class CallCold(meth: Symbol, source: Tree, trace: Seq[Tree]) extends Error {
    def show(using Context): String =
      "Call method " + source.show + " on a value with an unknown initialization" + "."
  }

  case class CallUnknown(meth: Symbol, source: Tree, trace: Seq[Tree]) extends Error {
    def show(using Context): String =
      "Unable to analyze external " + meth.show + "."
  }

  /** Promote a value under initialization to fully-initialized */
  case class UnsafePromotion(pot: Potential, source: Tree, trace: Seq[Tree], errors: Errors) extends Error {
    assert(errors.nonEmpty)

    override def issue(using Context): Unit =
      report.warning(show, source.srcPos)

    def show(using Context): String = {
      var index = 0
      "Promoting the value to fully-initialized is unsafe. " + stacktrace +
        "\nThe unsafe promotion may cause the following problem(s):\n" +
        (errors.flatMap(_.flatten).map { error =>
          index += 1
          s"\n$index. " + error.show + error.stacktrace
        }.mkString)
    }
  }
}