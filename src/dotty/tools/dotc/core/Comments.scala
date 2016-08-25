package dotty.tools
package dotc
package core

import dotc.ast.{ untpd, tpd }
import Decorators._
import Symbols._
import Contexts.Context
import Flags.EmptyFlags
import dotc.util.SourceFile
import dotc.util.Positions._
import dotc.parsing.Parsers.Parser
import dotty.tools.dottydoc.model.comment.CommentUtils._

object Comments {

  case class Comment(pos: Position, raw: String)(implicit ctx: Context) {
    val isDocComment = raw.startsWith("/**")

    private[this] lazy val sections = tagIndex(raw)

    private def fold[A](z: A)(op: => A) = if (!isDocComment) z else op

    lazy val usecases = fold(List.empty[UseCase]) {
      sections
        .filter { startsWithTag(raw, _, "@usecase") }
        .map { case (start, end) => decomposeUseCase(start, end) }
    }

    /** Turns a usecase section into a UseCase, with code changed to:
     *  {{{
     *  // From:
     *  def foo: A
     *  // To:
     *  def foo: A = ???
     *  }}}
     */
    private def decomposeUseCase(start: Int, end: Int): UseCase = {
      val codeStart    = skipWhitespace(raw, start + "@usecase".length)
      val codeEnd      = skipToEol(raw, codeStart)
      val code         = raw.substring(codeStart, codeEnd) + " = ???"
      val codePos      = subPos(codeStart, codeEnd)
      val commentStart = skipLineLead(raw, codeEnd + 1) min end
      val comment      = "/** " + raw.substring(commentStart, end) + "*/"
      val commentPos   = subPos(commentStart, end)

      UseCase(Comment(commentPos, comment), code, codePos)
    }

    private def subPos(start: Int, end: Int) =
      if (pos == NoPosition) NoPosition
      else {
        val start1 = pos.start + start
        val end1 = pos.end + end
        pos withStart start1 withPoint start1 withEnd end1
      }
  }

  case class UseCase(comment: Comment, code: String, codePos: Position)(implicit ctx: Context) {
    /** Entered by Namer */
    var symbol: Symbol = _

    /** Set by typer */
    var tpdCode: tpd.DefDef = _

    lazy val untpdCode: untpd.Tree = {
      val tree = new Parser(new SourceFile("<usecase>", code)).localDef(codePos.start, EmptyFlags)

      tree match {
        case tree: untpd.DefDef =>
          val newName = (tree.name.show + "$" + codePos.start).toTermName
          untpd.DefDef(newName, tree.tparams, tree.vparamss, tree.tpt, tree.rhs)
        case _ =>
          ctx.error("proper definition was not found in `@usecase`", codePos)
          tree
      }
    }
  }
}
