package dotty.dokka
package site

import java.io.File
import java.nio.file.Files

import com.vladsch.flexmark.ext.anchorlink.AnchorLinkExtension
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.emoji.EmojiExtension
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.ext.yaml.front.matter.{AbstractYamlFrontMatterVisitor, YamlFrontMatterExtension}
import com.vladsch.flexmark.parser.{Parser, ParserEmulationProfile}
import com.vladsch.flexmark.util.options.{DataHolder, MutableDataSet}
import org.jetbrains.dokka.links.{DRI, PointingToDeclaration}
import org.jetbrains.dokka.model.doc.Text

import scala.collection.JavaConverters._

val docsRootDRI: DRI = mkDRI(extra = "_top_level_index")
val docsDRI: DRI = mkDRI(extra = "_docs_level_index")
val apiPageDRI: DRI = mkDRI(packageName = "api", extra = "__api__")

val defaultMarkdownOptions: DataHolder =
  new MutableDataSet()
    .setFrom(ParserEmulationProfile.KRAMDOWN.getOptions)
    .set(
      Parser.EXTENSIONS, List(
        TablesExtension.create(),
        TaskListExtension.create(),
        AutolinkExtension.create(),
        AnchorLinkExtension.create(),
        EmojiExtension.create(),
        YamlFrontMatterExtension.create(),
        StrikethroughExtension.create()
      ).asJava)
    .set(
      EmojiExtension.ROOT_IMAGE_PATH,
      "https://github.global.ssl.fastly.net/images/icons/emoji/"
    )

def emptyTemplate(file: File): TemplateFile = TemplateFile(
  file = file,
  isHtml = true,
  rawCode = "",
  settings = Map.empty
)

final val ConfigSeparator = "---"
final val LineSeparator = "\n"

val yamlParser: Parser = Parser.builder(defaultMarkdownOptions).build()

def loadTemplateFile(file: File): TemplateFile = {
  val lines = Files.readAllLines(file.toPath).asScala.toList

  val (config, content) = if (lines.head == ConfigSeparator) {
    // Taking the second occurrence of ConfigSeparator.
    // The rest may appear within the content.
    val index = lines.drop(1).indexOf(ConfigSeparator) + 2
    (lines.take(index), lines.drop(index))
  } else (Nil, lines)

  val configParsed = yamlParser.parse(config.mkString(LineSeparator))
  val yamlCollector = new AbstractYamlFrontMatterVisitor()
  yamlCollector.visit(configParsed)

  extension (v: java.util.List[String]) def getSettingValue: String | List[String] =
    if v.size == 1 then v.get(0) else v.asScala.toList

  val globalKeys = Set("extraJS", "extraCSS", "layout", "hasFrame")
  val (global, inner) = yamlCollector.getData.asScala.toMap.transform((_, v) => v.getSettingValue)
    .partition((k,_) => globalKeys.contains(k))
  val settings = global ++ Map("page" -> inner)
 
  TemplateFile(
    file = file,
    isHtml = file.getName.endsWith(".html"),
    rawCode = content.mkString(LineSeparator),
    settings = settings,
  )
}

def Text(msg: String = "") = new Text(msg, JList(), JMap())
