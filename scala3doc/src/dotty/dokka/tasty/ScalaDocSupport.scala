package dotty.dokka.tasty

import scala.jdk.CollectionConverters._

import org.jetbrains.dokka.model.{doc => dkkd}

import dotty.dokka.Args.CommentSyntax
import dotty.dokka.ScalaTagWrapper
import comments.{kt, dkk}

trait ScaladocSupport { self: TastyParser =>
  import qctx.reflect._

  def parseComment(
    commentPre: Documentation,
    tree: Tree
  ): dkkd.DocumentationNode = {
    val commentNode =
      if tree.symbol.isClassDef || tree.symbol.owner.isClassDef then
        import dotty.tools.dotc
        given ctx as dotc.core.Contexts.Context = qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx

        // println(s"Expanding comment for sym: ${tree.symbol.show}")
        val sym = tree.symbol.asInstanceOf[dotc.core.Symbols.Symbol]
        val owner =
          if tree.symbol.isClassDef then sym else sym.owner

        comments.CommentExpander.cookComment(sym, owner)(using ctx)
          .get.asInstanceOf[Documentation]
      else
        commentPre

    val commentString = commentNode.expanded getOrElse commentNode.raw
    val preparsed =
      comments.Preparser.preparse(comments.Cleaner.clean(commentString))

    val commentSyntax =
      preparsed.syntax.headOption match {
        case Some(commentSetting) =>
          CommentSyntax.fromString(commentSetting).getOrElse {
            println(s"WARN: not a valid comment syntax: $commentSetting")
            println(s"WARN: Defaulting to Markdown syntax.")
            CommentSyntax.Markdown
          }
        case None => defaultCommentSyntax
      }

    val parser = commentSyntax match {
      case CommentSyntax.Wiki =>
        comments.WikiCommentParser(comments.Repr(qctx)(tree.symbol))
      case CommentSyntax.Markdown =>
        comments.MarkdownCommentParser(comments.Repr(qctx)(tree.symbol))
    }
    val parsed = parser.parse(preparsed)

    import kotlin.collections.builders.{ListBuilder => KtListBuilder}
    val bld = new KtListBuilder[dkkd.TagWrapper]
    parsed.short match {
      case Some(tag) => bld.add(dkkd.Description(tag))
      case None => bld.add(dkkd.Description(dkk.text("")))
    }
    bld.add(dkkd.Description(parsed.body))

    inline def addOpt(opt: Option[dkkd.DocTag])(wrap: dkkd.DocTag => dkkd.TagWrapper) =
      opt.foreach { t => bld.add(wrap(t)) }

    inline def addSeq[T](seq: Iterable[T])(wrap: T => dkkd.TagWrapper) =
      seq.foreach { t => bld.add(wrap(t)) }

    // this is a total kludge, this should be done in a deeper layer but we'd
    // need to refactor code there first
    def correctParagraphTags(tag: dkkd.DocTag): dkkd.DocTag =
      tag match {
        case tag: dkkd.P =>
          // NOTE we recurse once, since both the top-level element and its children can be P
          // (there is no special root DocTag)
          dkkd.Span(tag.getChildren.iterator.asScala.map(correctParagraphTags).toSeq.asJava, tag.getParams)
        case tag => tag
      }

    addSeq(parsed.authors)(dkkd.Author(_))
    addOpt(parsed.version)(dkkd.Version(_))
    addOpt(parsed.since)(dkkd.Since(_))
    addOpt(parsed.deprecated)(dkkd.Deprecated(_))
    addSeq(parsed.todo)(ScalaTagWrapper.Todo)
    addSeq(parsed.see)(ScalaTagWrapper.See)
    addSeq(parsed.note)(ScalaTagWrapper.Note)
    addSeq(parsed.example)(ScalaTagWrapper.Example)

    addOpt(parsed.constructor)(dkkd.Constructor(_))
    addSeq(parsed.valueParams){ case (name, tag) =>
      ScalaTagWrapper.NestedNamedTag("Param", name, dkk.text(name), correctParagraphTags(tag))
    }
    addSeq(parsed.typeParams){ case (name, tag) =>
      ScalaTagWrapper.NestedNamedTag("Type param", name, dkk.text(name), correctParagraphTags(tag))
    }
    addSeq(parsed.throws){ case (key, (exc, desc)) =>
      ScalaTagWrapper.NestedNamedTag("Throws", key, exc, correctParagraphTags(desc))
    }
    addOpt(parsed.result)(dkkd.Return(_))

    new dkkd.DocumentationNode(bld.build())
  }

  private val defaultCommentSyntax =
    self.config.docConfiguration.args.defaultSyntax getOrElse CommentSyntax.Markdown
}
