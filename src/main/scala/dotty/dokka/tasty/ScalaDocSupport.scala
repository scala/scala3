package dotty.dokka.tasty

import scala.tasty.reflect._
import org.jetbrains.dokka.model.{doc => dokkaDoc}
import kotlin.collections.builders.{ListBuilder => KtListBuilder, MapBuilder => KtMapBuilder}

trait ScaladocSupport { self: TastyParser =>
  import reflect.{given _, _}

  def parseComment(
    commentNode: reflect.Comment,
    tree: reflect.Tree
  ): dokkaDoc.DocumentationNode = {

    val preParsed = comment.CommentParser.parse(comment.CommentCleaner.clean(commentNode.raw))
    val parsed = comment.MarkdownComment((), preParsed, ()).comment

    def ktEmptyList[T]() = new KtListBuilder[T]().build()
    def ktEmptyMap[A, B]() = new KtMapBuilder[A, B]().build()

    object dokkaTag {
      def text(str: String) =
        dokkaDoc.Text(str, ktEmptyList(), ktEmptyMap())
    }

    val bld = new KtListBuilder[dokkaDoc.TagWrapper]
    bld.add(dokkaDoc.Description(dokkaTag.text(parsed.body)))
    bld.add(dokkaDoc.Description(dokkaTag.text("hello world")))

    inline def addOpt(opt: Option[String])(wrap: dokkaDoc.DocTag => dokkaDoc.TagWrapper) =
      opt.foreach { t => bld.add(wrap(dokkaTag.text(t))) }

    inline def addSeq(seq: Seq[String])(wrap: dokkaDoc.DocTag => dokkaDoc.TagWrapper) =
      seq.foreach { t => bld.add(wrap(dokkaTag.text(t))) }

    addSeq(parsed.authors)(dokkaDoc.Author(_))
    addOpt(parsed.version)(dokkaDoc.Version(_))
    addOpt(parsed.since)(dokkaDoc.Since(_))
    addOpt(parsed.result)(dokkaDoc.Return(_))

    val res = new dokkaDoc.DocumentationNode(bld.build())

    res

    // inspector.parser.parse(commentNode.raw) // TODO use scaladoc reader to generate proper docs
  }
}
