package dotty.tools
package dotc
package typer

import ast.*
import Trees.*
import core.*, Contexts.*
import Constants.*
import Symbols.*
import parsing.Parsers.Parser
import parsing.Tokens
import util.SourceFile
import dotty.tools.dotc.reporting.{Diagnostic, Reporter}
import dotty.tools.dotc.printing.Formatting.ShownDef.Shown.runCtxShow

/** Spec strings a representation of programmer's intent that guides
 *  completions by coding agents. They are enabled under the experimental
 *  `magic` language import:
 *
 *     import language.experimental.magic
 *
 *  The syntax of a spec strings is like a dedented string literal (see SIP 72) where the
 *  opening quote is followed by `spec`. Example:
 *
 *    '''spec
 *    Parse date string into `Date` structure
 *    '''
 *
 *  Spec strings are formatted in markdown syntax. The result of parsing and typing
 *  a spec string is an interpolated string literal. Like other interpolated  literals
 *  this is represented internally by a selection identifier (`$spec` in this case) on
 *  a string context that represents a string with holes and typed argument expressions
 *  that go in the holes.
 *
 *  The backtick code regions in markdown trigger "soft" string interpolations. That is,
 *  the compiler will try to parse, resolve and type-check the identifiers, expressions,
 *  or types in backticks and, if successful, turn them into interpolated expressions
 *  embedded in the string. IDEs should render well-typed content differently from ill-typed
 *  content. Well-typed content supports full editing actions including hyperlinking and
 *  global renaming. Ill-typed content still lets the program as a whole compile, but hover
 *  on such content could reveal error messages such as "not found" or "type mismatch"
 *  as warnings.
 *
 *  For instance, we could have defined a format for dates somewhere under the
 *  name `formatSchema`. Then the following spec string could refer to it like this:
 *
 *    '''spec
 *    Parse date string into a `Date` structure. The string is in the format given by `formatSchema`.
 *    '''
 *
 *  This string will be represented internally as follows:
 *
 *     scala.compiletime.$spec(
 *       StringContext(
 *         "    \n   Parse date string into `",
 *         "` structure. The string is in the format given by `",
 *         "`.\n    "
 *       )(scala.compiletime.$wrappeType[Date], formatString)
 *
 *  Notes:
 *
 *    - The compiler can parse single line or mutli-line code blocks
 *    - Each code block is first attempted to be parsed and typechecked
 *      as the contents of a `{...}` block. If that succeeds, the expression
 *      if treated as interpolated argument. E.g. formatString in the previous
 *      example. If parsing or typechecking as an expression fails, the code
 *      is parsed and typechecked as a type. If that succeeds, the type is
 *      wrapped in `compiletime.$wrappedType` and treated as an interpolated
 *      argument. E.g. Date in the previous example.
 *    - If neither parse and typecheck works, the errors of the first attempt,
 *      when the code block was treated as an expression, are produced as warnings
 *      whereas the code block is kept without extraction in the surrounding string.
 *    - The compiletime $spec function is a unit-valued inline function that collapses
 *      calls to just `()`. That is, spec strings have no significant runtime
 *      representation.
 *    - The purpose of the compile-time representation of a spec string is as
 *      a means to support navigation by users and to keep the program consistent
 *      under use actions such as global renames.
 */
trait SpecStrings { this: Typer =>
  import tpd.*

  private def reportWarnings(nestedCtx: Context)(using Context): Unit =
    nestedCtx.reporter.mapBufferedMessages:
      case err: Diagnostic.Error => Diagnostic.Warning(err.msg, err.pos)
      case dia => dia
    nestedCtx.reporter.flush()

  type QuoteSpan = (start: Int, end: Int, nquotes: Int)

  /** The interval defining the first code in `...` or ```...``` */
  def firstQuoteSpan(str: String, startFrom: Int): Option[QuoteSpan] =
    val opening = str.indexOf('`', from = startFrom)
    if opening >= 0 then
      var start = opening + 1
      while start < str.length && str(start) == '`' do start += 1
      val nquotes = start - opening

      def findClosing(start: Int): Option[QuoteSpan] =
        val closing = str.indexOf('`', from = start)
        if closing < 0 then None
        else if str.startsWith("`" * nquotes, closing) then Some((opening, closing, nquotes))
        else findClosing(closing + 1)

      findClosing(start)
    else None

  /** Try to analyze code block between `start` and `end` source offsets, using
   *  parsing operation `parse` and type-checking operation `typeCheck`. If
   *  successful, return the resulting tree as a TypedSplice tree. If
   *  there were errors return a context containing the error reporter.
   */
  def tryAnalyze(start: Int, end: Int,
      parse: Parser => untpd.Tree,
      typeCheck: untpd.Tree => Context ?=> tpd.Tree)(using Context): untpd.TypedSplice | Context =
    val nestedCtx = ctx.fresh.setNewTyperState()
    inContext(nestedCtx):
      val parser = Parser(ctx.compilationUnit.source, start, end)
      val untpdExpr = parse(parser)
      parser.accept(Tokens.EOF)
      if ctx.reporter.hasErrors then nestedCtx
      else
        val tpdExpr = typeCheck(untpdExpr)
        if ctx.reporter.hasErrors then nestedCtx
        else untpd.TypedSplice(tpdExpr)

  /** Try to extract all backquoted code blocks in `strLit`, starting from
   *  the startFrom index of the contained string.
   */
  private def extractBackquoted(strLit: untpd.Literal, startFrom: Int = 0)(using Context): List[untpd.Tree] =
    val Literal(Constant(str: String)) = strLit.runtimeChecked
    firstQuoteSpan(str, startFrom) match
      case Some((opening, closing, nquotes)) =>
        val startOffset = strLit.span.start
        val startParse = startOffset + opening + nquotes
        val endParse = startOffset + closing
        println(s"str /$str/ starting at ${ctx.compilationUnit.source.content().slice(startOffset, startOffset + 10).mkString("[",",","]")}")
        println(s"starting $nquotes at ${ctx.compilationUnit.source.content().slice(startParse, startParse + 10).mkString("[",",","]")}")

        def extract(tree: untpd.Tree) =
          val prefix = Literal(Constant(str.take(opening + 1)))
              .withSpan(strLit.span.withEnd(startOffset + opening + nquotes))
          val suffix = Literal(Constant(str.drop(closing)))
              .withSpan(strLit.span.withStart(startOffset + closing))
          untpd.Thicket(prefix, tree) :: extractBackquoted(suffix, nquotes)

        tryAnalyze(startParse, endParse, _.block(simplify = true), typed(_)) match
          case failCtx: Context =>
            tryAnalyze(startParse, endParse, _.typ(), typedType(_)) match
              case failCtx2: Context =>
                reportWarnings(failCtx)
                extractBackquoted(strLit, closing + 1)
              case untpd.TypedSplice(splice) =>
                extract(
                  untpd.TypedSplice(ref(defn.Compiletime_wrappedType)
                    .appliedToTypeTree(splice)))
          case tree: untpd.TypedSplice =>
            extract(tree)
      case None => strLit :: Nil

  /** Process an interpolated spec string segment */
  def processSpecSegment(tree: untpd.Tree)(using Context): List[untpd.Tree] =
    tree match
    case Thicket(strLit :: expr :: Nil) =>
      processSpecSegment(strLit).runtimeChecked match
        case _ :: Nil => tree :: Nil  // no change
        case prefix :+ last => prefix :+ untpd.Thicket(last :: expr :: Nil)
    case strLit: Literal =>
      extractBackquoted(strLit, 0)

  /** Process an interpolated spec string consisting of given segments */
  def processSpec(segments: List[untpd.Tree])(using Context): List[untpd.Tree] =
    segments.flatMap(processSpecSegment)
}

