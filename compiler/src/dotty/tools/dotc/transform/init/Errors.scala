package dotty.tools.dotc
package transform
package init

import ast.tpd._

import core._
import Decorators._
import Types._, Symbols._, Contexts._

import Effects._, Potentials._

object Errors {
  type Errors = Set[Error]
  val empty: Errors = Set.empty

  sealed trait Error {
    def trace: Vector[Tree]
    def report(implicit ctx: Context): Unit
    def message(implicit ctx: Context): String

    def stacktrace(implicit ctx: Context): String = {
      var indentCount = 0
      var last = ""
      val sb = new StringBuilder
      trace.foreach { tree =>
        indentCount += 1
        val pos = tree.sourcePos
        val line = "[ " + pos.source.file.name + ":" + (pos.line + 1) + " ]"
        if (last != line)
          sb.append(
            if (pos.source.exists)
              i"${ " " * indentCount }-> ${pos.lineContent.trim}\t$line\n"
            else
              i"${tree.show}\n"
          )
        last = line
      }
      sb.toString
    }
  }

  /** Access non-initialized field */
  case class AccessNonInit(field: Symbol, trace: Vector[Tree]) extends Error {
    def message(implicit ctx: Context): String =
      "Access non-initialized field " + field.show + ". Calling trace:\n" + stacktrace

    def report(implicit ctx: Context): Unit = ???
  }

  /** Promote `this` under initialization to fully-initialized */
  case class PromoteThis(pot: ThisRef, source: Tree, trace: Vector[Tree]) extends Error {
    def message(implicit ctx: Context): String = "Promote `this` to be initialized while it is not. Calling trace:\n" + stacktrace
    def report(implicit ctx: Context): Unit = ???
  }

  /** Promote a cold value under initialization to fully-initialized */
  case class PromoteCold(source: Tree, trace: Vector[Tree]) extends Error {
    def message(implicit ctx: Context): String =
      "Promoting the value " + source.show + " to be initialized while it is under initialization" +
      ". Calling trace:\n" + stacktrace

    def report(implicit ctx: Context): Unit = ???
  }

  case class AccessCold(field: Symbol, source: Tree, trace: Vector[Tree]) extends Error {
    def message(implicit ctx: Context): String =
      "Access field " + source.show + " on a value under unknown initialization status" +
      ". Calling trace:\n" + stacktrace

    def report(implicit ctx: Context): Unit = ???
  }

  case class CallCold(meth: Symbol, source: Tree, trace: Vector[Tree]) extends Error {
    def message(implicit ctx: Context): String =
      "Call method " + source.show + " on a value under unknown initialization" +
      ". Calling trace:\n" + stacktrace

    def report(implicit ctx: Context): Unit = ???
  }

  case class CallUnknown(meth: Symbol, source: Tree, trace: Vector[Tree]) extends Error {
    def message(implicit ctx: Context): String =
      "Calling the external method " + meth.show +
      " may cause initialization errors" + ". Calling trace:\n" + stacktrace

    def report(implicit ctx: Context): Unit = ???
  }

  /** Promote a value under initialization to fully-initialized */
  case class UnsafePromotion(pot: Potential, source: Tree, trace: Vector[Tree], errors: Set[Error]) extends Error {
    def message(implicit ctx: Context): String = ???

    def report(implicit ctx: Context): Unit = ???
  }
}