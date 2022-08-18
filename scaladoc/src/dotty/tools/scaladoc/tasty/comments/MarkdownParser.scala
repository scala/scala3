package dotty.tools.scaladoc
package tasty.comments

import java.util.{ Arrays }
import Regexes._

import com.vladsch.flexmark.util.{ast => mdu}
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.sequence.CharSubSequence
import com.vladsch.flexmark.parser.ParserEmulationProfile
import com.vladsch.flexmark.ext.gfm.tables.TablesExtension
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.emoji.EmojiExtension
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.anchorlink.AnchorLinkExtension
import com.vladsch.flexmark.ext.yaml.front.matter.YamlFrontMatterExtension
import com.vladsch.flexmark.ext.wikilink.WikiLinkExtension
import com.vladsch.flexmark.util.options.{ DataHolder, MutableDataSet }
import com.vladsch.flexmark.util.builder.Extension

import scala.jdk.CollectionConverters._

object MarkdownParser {

  def mkMarkdownOptions(additionalExtensions: Seq[Extension]) =
    val extArray = Seq(
      TablesExtension.create(),
      TaskListExtension.create(),
      AutolinkExtension.create(),
      AnchorLinkExtension.create(),
      EmojiExtension.create(),
      YamlFrontMatterExtension.create(),
      StrikethroughExtension.create()
    ) ++ additionalExtensions

    new MutableDataSet()
      .setFrom(ParserEmulationProfile.COMMONMARK.getOptions)
      .set(Parser.EXTENSIONS, Arrays.asList(extArray:_*))
      .set(EmojiExtension.ROOT_IMAGE_PATH,
        "https://github.global.ssl.fastly.net/images/icons/emoji/")
      .set(WikiLinkExtension.LINK_ESCAPE_CHARS, "")

  val markdownOptions: DataHolder = mkMarkdownOptions(Seq(WikiLinkExtension.create()))

  val RENDERER = Formatter.builder(markdownOptions).build()

  def parseToMarkdown(text: String, extensions: Extension*): mdu.Document =
    val options =
      if extensions.isEmpty then
        markdownOptions
      else
        mkMarkdownOptions(extensions)

    val thisParser =
      Parser.builder(options)
        .customBlockParserFactory(parsers.WikiCodeBlockParser.Factory())
        .build()

    // We need to remove safe tag markers as they break flexmark parsing
    thisParser.parse(text.replace(safeTagMarker.toString, "")).asInstanceOf[mdu.Document]


  def renderToText(node: mdu.Node): String =
    RENDERER.render(node)
}
