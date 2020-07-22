package dotty.dokka.tasty

import scala.jdk.CollectionConverters._
import scala.tasty.reflect._

import kotlin.collections.builders.{ListBuilder => KtListBuilder, MapBuilder => KtMapBuilder}

import org.jetbrains.dokka.model.{doc => dokkaDoc}
import com.vladsch.flexmark.util.{ast => mdu}

trait ScaladocSupport { self: TastyParser =>
  import reflect.{given _, _}

  def parseComment(
    commentNode: reflect.Comment,
    tree: reflect.Tree
  ): dokkaDoc.DocumentationNode = {

    val preparsed = comments.Preparser.preparse(comments.Cleaner.clean(commentNode.raw))
    val parser = comments.MarkdownParser((), ())
    val parsed = parser.parse(preparsed)

    def ktEmptyList[T]() = new KtListBuilder[T]().build()
    def ktEmptyMap[A, B]() = new KtMapBuilder[A, B]().build()

    object dokkaTag {
      def text(str: String) =
        dokkaDoc.Text(str, ktEmptyList(), ktEmptyMap())
    }

    val bld = new KtListBuilder[dokkaDoc.TagWrapper]
    // TODO determine how to have short and long descriptions
    bld.add(dokkaDoc.Description(comments.MarkdownConverter.convertNode(
      parser.stringToMarkup(preparsed.body)
    )))

    inline def addOpt(opt: Option[String])(wrap: dokkaDoc.DocTag => dokkaDoc.TagWrapper) =
      opt.foreach { t => bld.add(wrap(dokkaTag.text(t))) }

    inline def addSeq(seq: Seq[String])(wrap: dokkaDoc.DocTag => dokkaDoc.TagWrapper) =
      seq.foreach { t => bld.add(wrap(dokkaTag.text(t))) }

    addSeq(parsed.authors)(dokkaDoc.Author(_))
    addOpt(parsed.version)(dokkaDoc.Version(_))
    addOpt(parsed.since)(dokkaDoc.Since(_))
    addOpt(parsed.constructor)(dokkaDoc.Constructor(_))
    addOpt(parsed.result)(dokkaDoc.Return(_)) // does not seem to render for classes, intentional?

    new dokkaDoc.DocumentationNode(bld.build())
  }
}
