package dotty.dokka.tasty

import scala.jdk.CollectionConverters._
import scala.tasty.reflect._

import org.jetbrains.dokka.model.{doc => dkkd}

import comments.{kt, dkk}

trait ScaladocSupport { self: TastyParser =>
  import reflect.{given _, _}

  def parseComment(
    commentNode: reflect.Comment,
    tree: reflect.Tree
  ): dkkd.DocumentationNode = {
    val preparsed =
      comments.Preparser.preparse(comments.Cleaner.clean(commentNode.raw))
    val parser =
      if preparsed.syntax.headOption.contains("wiki") then
        comments.WikiCommentParser(comments.Repr(reflect)(tree.symbol), ())
      else
        comments.MarkdownCommentParser(comments.Repr(reflect)(tree.symbol), ())
    val parsed = parser.parse(preparsed)

    import kotlin.collections.builders.{ListBuilder => KtListBuilder}
    val bld = new KtListBuilder[dkkd.TagWrapper]
    // TODO determine how to have short and long descriptions
    bld.add(dkkd.Description(parsed.body))

    inline def addOpt(opt: Option[dkkd.DocTag])(wrap: dkkd.DocTag => dkkd.TagWrapper) =
      opt.foreach { t => bld.add(wrap(t)) }

    inline def addSeq(seq: Seq[dkkd.DocTag])(wrap: dkkd.DocTag => dkkd.TagWrapper) =
      seq.foreach { t => bld.add(wrap(t)) }

    addSeq(parsed.authors)(dkkd.Author(_))
    addOpt(parsed.version)(dkkd.Version(_))
    addOpt(parsed.since)(dkkd.Since(_))
    addOpt(parsed.constructor)(dkkd.Constructor(_))
    addOpt(parsed.result)(dkkd.Return(_)) // does not seem to render for classes, intentional?

    new dkkd.DocumentationNode(bld.build())
  }
}
