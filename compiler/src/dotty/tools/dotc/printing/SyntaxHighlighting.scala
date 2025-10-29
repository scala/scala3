package dotty.tools.dotc
package printing

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Scanners.Scanner
import dotty.tools.dotc.parsing.Tokens.*
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SourceFile

import dotty.shaded.fansi
import scala.collection.mutable.ArrayBuffer

/** This object provides functions for syntax highlighting in the REPL */
object SyntaxHighlighting {

  /** The name of the virtual source file used for highlighting */
  val VirtualSourceName = "<highlighting>"

  /** if true, log erroneous positions being highlighted */
  private inline val debug = true

  // Keep in sync with SyntaxHighlightingTests
  val NoColor: fansi.Attrs         = fansi.Attrs.Empty
  val CommentColor: fansi.Attrs    = fansi.Color.Blue
  val KeywordColor: fansi.Attrs    = fansi.Color.Yellow
  val ValDefColor: fansi.Attrs     = fansi.Color.Cyan
  val LiteralColor: fansi.Attrs    = fansi.Color.Green
  val StringColor: fansi.Attrs     = fansi.Color.Green
  val TypeColor: fansi.Attrs       = fansi.Color.Magenta
  val AnnotationColor: fansi.Attrs = fansi.Color.Magenta

  def highlight(in: String)(using Context): String = {
    def freshCtx = ctx.fresh.setReporter(Reporter.NoReporter)
    if (in.isEmpty || ctx.settings.color.value == "never") in
    else {
      val source = SourceFile.virtual(VirtualSourceName, in)

      given Context = freshCtx
        .setCompilationUnit(CompilationUnit(source, mustExist = false)(using freshCtx))

      // Buffer to collect all highlight ranges for fansi.Str.overlayAll
      val ranges = ArrayBuffer[(fansi.Attrs, Int, Int)]()

      def highlightRange(from: Int, to: Int, attrs: fansi.Attrs) =
        if (attrs != fansi.Attrs.Empty)
          ranges += ((attrs, from, to))

      def highlightPosition(span: Span, attrs: fansi.Attrs) = if (span.exists)
        if (span.start < 0 || span.end > in.length) {
          if (debug)
            println(s"Trying to highlight erroneous position $span. Input size: ${in.length}")
        }
        else
          highlightRange(span.start, span.end, attrs)

      val scanner = new Scanner(source)
      while (scanner.token != EOF) {
        val start = scanner.offset
        val token = scanner.token
        val name = scanner.name
        val isSoftModifier = scanner.isSoftModifierInModifierPosition
        scanner.nextToken()
        val end = scanner.lastOffset

        // Branch order is important. For example,
        // `true` is at the same time a keyword and a literal
        token match {
          case _ if literalTokens.contains(token) =>
            highlightRange(start, end, LiteralColor)

          case STRINGPART =>
            // String interpolation parts include `$` but
            // we don't highlight it, hence the `-1`
            highlightRange(start, end - 1, LiteralColor)

          case _ if alphaKeywords.contains(token) || isSoftModifier =>
            highlightRange(start, end, KeywordColor)

          case IDENTIFIER if name == nme.??? =>
            highlightRange(start, end, fansi.Back.Red)

          case IDENTIFIER if name.head.isUpper && name.exists(!_.isUpper) =>
            highlightRange(start, end, KeywordColor)

          case _ =>
        }
      }

      for (comment <- scanner.comments)
        highlightPosition(comment.span, CommentColor)

      object TreeHighlighter extends untpd.UntypedTreeTraverser {
        import untpd.*

        def ignored(tree: NameTree) = {
          val name = tree.name.toTermName
          // trees named <error> and <init> have weird positions
          name == nme.ERROR || name == nme.CONSTRUCTOR
        }

        def highlightAnnotations(tree: MemberDef): Unit =
          for (annotation <- tree.rawMods.annotations)
            highlightPosition(annotation.span, AnnotationColor)

        def highlight(trees: List[Tree])(using Context): Unit =
          trees.foreach(traverse)

        def traverse(tree: Tree)(using Context): Unit = {
          tree match {
            case tree: NameTree if ignored(tree) =>
              ()
            case tree: ValOrDefDef =>
              highlightAnnotations(tree)
              highlightPosition(tree.nameSpan, ValDefColor)
              highlightPosition(tree.endSpan, ValDefColor)
            case tree: MemberDef /* ModuleDef | TypeDef */ =>
              highlightAnnotations(tree)
              highlightPosition(tree.nameSpan, TypeColor)
              highlightPosition(tree.endSpan, TypeColor)
            case tree: Ident if tree.isType =>
              highlightPosition(tree.span, TypeColor)
            case _: TypeTree =>
              highlightPosition(tree.span, TypeColor)
            case _ =>
          }
          traverseChildren(tree)
        }
      }

      try
        val parser = new Parser(source)
        val trees = parser.blockStatSeq()
        TreeHighlighter.highlight(trees)

        // Apply all color ranges at once using fansi.Str.overlayAll
        fansi.Str(in).overlayAll(ranges.toSeq).render
      catch
        case e: StackOverflowError =>
          in
    }
  }

}
