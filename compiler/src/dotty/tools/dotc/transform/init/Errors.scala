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
    def report(implicit ctx: Context): Unit = ???
  }

  /** Promote `this` under initialization to fully-initialized */
  case class PromoteThis(pot: ThisRef, trace: Vector[Tree]) extends Error {
    def report(implicit ctx: Context): Unit = ???
  }

  /** Promote a cold value under initialization to fully-initialized */
  case class PromoteCold(trace: Vector[Tree]) extends Error {
    def report(implicit ctx: Context): Unit = ???
  }

  /** Promote a value under initialization to fully-initialized */
  case class UnsafePromotion(pot: Potential, trace: Vector[Tree], errors: Set[Error]) extends Error {
    def report(implicit ctx: Context): Unit = ???
  }
}