package dotty.dokka.tasty.comments

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

object MarkdownParser {

  val markdownOptions: DataHolder =
    new MutableDataSet()
      .setFrom(ParserEmulationProfile.KRAMDOWN.getOptions)
      .set(Parser.EXTENSIONS, Arrays.asList(
        TablesExtension.create(),
        TaskListExtension.create(),
        AutolinkExtension.create(),
        AnchorLinkExtension.create(),
        EmojiExtension.create(),
        YamlFrontMatterExtension.create(),
        StrikethroughExtension.create(),
        WikiLinkExtension.create(),
      ))
      .set(EmojiExtension.ROOT_IMAGE_PATH,
        "https://github.global.ssl.fastly.net/images/icons/emoji/")
      .set(WikiLinkExtension.LINK_ESCAPE_CHARS, "")

  val parser = Parser.builder(markdownOptions).build()

  val RENDERER = Formatter.builder(markdownOptions).build()

  def parseToMarkdown(text: String): mdu.Document =
    // We need to remove safe tag markers as they break flexmark parsing
    parser.parse(text.replace(safeTagMarker.toString, "")).asInstanceOf[mdu.Document]


  def renderToText(node: mdu.Node): String =
    RENDERER.render(node)
}
