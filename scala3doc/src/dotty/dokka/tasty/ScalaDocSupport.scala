package dotty.dokka.tasty

import scala.jdk.CollectionConverters._

import dotty.dokka.Scala3doc.CommentSyntax
import dotty.dokka.tasty.comments.Comment

trait ScaladocSupport { self: TastyParser =>
  import qctx.reflect._

  def parseCommentString(comment: String, sym: Symbol, pos: Option[Position]): Comment =
    val preparsed = comments.Preparser.preparse(comments.Cleaner.clean(comment))

    val commentSyntax =
      preparsed.syntax.headOption match {
        case Some(commentSetting) =>
          CommentSyntax.parse(commentSetting).getOrElse {
            val msg = s"not a valid comment syntax: $commentSetting, defaulting to Markdown syntax."
            // we should update pos with span from documentation
            pos.fold(report.warning(msg))(report.warning(msg, _))

            CommentSyntax.default
          }
        case None => ctx.args.defaultSyntax
      }

    val parser = commentSyntax match {
      case CommentSyntax.Wiki =>
        comments.WikiCommentParser(comments.Repr(qctx)(sym))
      case CommentSyntax.Markdown =>
        comments.MarkdownCommentParser(comments.Repr(qctx)(sym))
    }
    parser.parse(preparsed)

  def parseComment(docstring: String,  tree: Tree): Comment =
    val commentString: String =
      if tree.symbol.isClassDef || tree.symbol.owner.isClassDef then
        import dotty.tools.dotc
        given ctx: dotc.core.Contexts.Context = qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx

        val sym = tree.symbol.asInstanceOf[dotc.core.Symbols.Symbol]

        comments.CommentExpander.cookComment(sym)(using ctx)
          .get.expanded.get
      else
        docstring

    parseCommentString(commentString, tree.symbol, Some(tree.pos))

}
