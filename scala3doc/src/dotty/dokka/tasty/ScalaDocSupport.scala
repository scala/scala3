package dotty.dokka.tasty

import scala.jdk.CollectionConverters._

import org.jetbrains.dokka.model.{doc => dkkd}

import dotty.dokka.Scala3doc.CommentSyntax
import dotty.dokka.ScalaTagWrapper
import comments.{kt, dkk}
import dotty.dokka.tasty.comments.Comment

trait ScaladocSupport { self: TastyParser =>
  import qctx.reflect._

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

    val preparsed =
      comments.Preparser.preparse(comments.Cleaner.clean(commentString))

    val commentSyntax =
      preparsed.syntax.headOption match {
        case Some(commentSetting) =>
          CommentSyntax.parse(commentSetting).getOrElse {
            val msg = s"not a valid comment syntax: $commentSetting, defaulting to Markdown syntax."
            // we should update pos with span from documentation
            report.warning(msg, tree.pos)
            CommentSyntax.default
          }
        case None => ctx.args.defaultSyntax
      }

    val parser = commentSyntax match {
      case CommentSyntax.Wiki =>
        comments.WikiCommentParser(comments.Repr(qctx)(tree.symbol))
      case CommentSyntax.Markdown =>
        comments.MarkdownCommentParser(comments.Repr(qctx)(tree.symbol))
    }
    parser.parse(preparsed)
}
