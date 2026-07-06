package dotty.tools.scaladoc
package tasty

import scala.jdk.CollectionConverters._

import dotty.tools.scaladoc.tasty.comments.{Comment, CommentSyntax}
import dotty.tools.scaladoc.tasty.SymOps.source

import scala.quoted._

object ScaladocSupport:

  def parseCommentString(using Quotes, DocContext)(comment: String, sym: reflect.Symbol, pos: Option[reflect.Position]): Comment =
    import reflect.report
    val preparsed = comments.Preparser.preparse(comments.Cleaner.clean(comment))

    def pathBasedCommentSyntax(): CommentSyntax =
      val path = sym.source.map(_.path)
      summon[DocContext].commentSyntaxArgs.get(path)

    val commentSyntax =
      preparsed.syntax.headOption match {
        case Some(commentSetting) =>
          CommentSyntax.CommentSyntaxParser.parse(commentSetting).getOrElse {
            val defaultSyntax = pathBasedCommentSyntax()
            val msg = s"not a valid comment syntax: $commentSetting, defaulting to ${defaultSyntax} syntax."
            // we should update pos with span from documentation
            pos.fold(report.warning(msg))(report.warning(msg, _))

            defaultSyntax
          }
        case None =>
          pathBasedCommentSyntax()
      }

    val parser = commentSyntax match {
      case CommentSyntax.Wiki =>
        comments.WikiCommentParser(comments.Repr(quotes)(sym))
      case CommentSyntax.Markdown =>
        comments.MarkdownCommentParser(comments.Repr(quotes)(sym))
    }
    parser.parse(preparsed)

  def parseComment(using Quotes, DocContext)(docstring: String, tree: reflect.Tree): Option[Comment] =
    val (commentString: String, commentOwner: reflect.Symbol) =
      if tree.symbol.isClassDef || tree.symbol.owner.isClassDef then
        import dotty.tools.dotc
        import dotty.tools.dotc.core.Comments.docCtx
        given ctx: dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx

        val docCtx = ctx.docCtx.get

        val sym = tree.symbol.asInstanceOf[dotc.core.Symbols.Symbol]

        /** A member without a docstring of its own inherits the comment of the nearest overridden symbol that has one.
         *  @see [[dotty.tools.dotc.core.Comments.CommentExpander CommentExpander.superComment]]
         */
        def hasDocstring(s: dotc.core.Symbols.Symbol) = docCtx.docstring(s).exists(_.raw.nonEmpty)

        val commentSym =
          if hasDocstring(sym) || !sym.denot.owner.isClass then sym
          else sym.denot.allOverriddenSymbols.find(hasDocstring).getOrElse(sym)

        (docCtx.templateExpander.expand(sym, sym.owner), commentSym.asInstanceOf[reflect.Symbol])
      else
        (docstring, tree.symbol)
    if commentString == ""
    then None
    else Some(parseCommentString(commentString, commentOwner, Some(tree.pos)))
