package dotty.tools.scaladoc
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
import com.vladsch.flexmark.ext.wikilink.WikiLinkExtension
import com.vladsch.flexmark.formatter.Formatter

import scala.collection.JavaConverters._

val docsRootDRI: DRI = DRI(location = "_docs/index", symbolUUID = staticFileSymbolUUID)
val apiPageDRI: DRI = DRI(location = "api/index")

def defaultMarkdownOptions(using ctx: StaticSiteContext): DataHolder =
  new MutableDataSet()
    .setFrom(ParserEmulationProfile.COMMONMARK.getOptions)
    .set(AnchorLinkExtension.ANCHORLINKS_WRAP_TEXT, false)
    .set(AnchorLinkExtension.ANCHORLINKS_ANCHOR_CLASS, "anchor")
    .set(EmojiExtension.ROOT_IMAGE_PATH, "https://github.global.ssl.fastly.net/images/icons/emoji/")
    .set(WikiLinkExtension.LINK_ESCAPE_CHARS, "")
    .set(Parser.EXTENSIONS, java.util.Arrays.asList(
      TablesExtension.create(),
      TaskListExtension.create(),
      AutolinkExtension.create(),
      AnchorLinkExtension.create(),
      EmojiExtension.create(),
      YamlFrontMatterExtension.create(),
      StrikethroughExtension.create(),
      WikiLinkExtension.create(),
      tasty.comments.markdown.SnippetRenderingExtension
    ))

def emptyTemplate(file: File, title: String): TemplateFile = TemplateFile(
  file = file,
  isHtml = true,
  rawCode = "",
  settings = Map.empty,
  name = file.getName.stripSuffix(".html"),
  title = TemplateName.FilenameDefined(title),
  hasFrame = true,
  resources = List.empty,
  layout = None,
  configOffset = 0
)

final val ConfigSeparator = "---"
final val LineSeparator = "\n"

def yamlParser(using ctx: StaticSiteContext): Parser = Parser.builder(defaultMarkdownOptions).build()

def loadTemplateFile(file: File, defaultTitle: Option[TemplateName] = None)(using ctx: StaticSiteContext): TemplateFile = {
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

  def getSettingValue(k: String, v: JList[String]): String | List[String] =
    if v.size == 1 then v.get(0) else v.asScala.toList

  val globalKeys = Set("extraJS", "extraCSS", "layout", "hasFrame", "name", "title")
  val allSettings = yamlCollector.getData.asScala.toMap.transform(getSettingValue)
  val (global, inner) = allSettings.partition((k,_) => globalKeys.contains(k))
  val settings = Map("page" -> inner) ++ global

  def stringSetting(settings: Map[String, Object], name: String): Option[String] = settings.get(name).map {
    case List(elem: String) => elem
    case elem: String => elem
    case other => throw new RuntimeException(s"Expected a string setting for $name in $file but got $other")
  }.map(_.stripPrefix("\"").stripSuffix("\""))

  def listSetting(settings: Map[String, Object], name: String): Option[List[String]] = settings.get(name).map {
    case elems: List[_] => elems.zipWithIndex.map {
      case (s: String, _) => s
      case (other, index) =>
        throw new RuntimeException(s"Expected a string at index $index for $name in $file but got $other")
    }
    case elem: String => List(elem)
    case other => throw new RuntimeException(s"Expected a list of string setting for $name in $file but got $other")
  }

  val isHtml = file.getName.endsWith(".html")
  val name = stringSetting(allSettings, "name").getOrElse(file.getName.stripSuffix(if (isHtml) ".html" else ".md"))

  TemplateFile(
    file = file,
    isHtml = isHtml,
    rawCode = content.mkString(LineSeparator),
    settings = settings,
    name = name,
    title = stringSetting(allSettings, "title").map(TemplateName.YamlDefined(_)).orElse(defaultTitle).getOrElse(TemplateName.FilenameDefined(name)),
    hasFrame = !stringSetting(allSettings, "hasFrame").contains("false"),
    resources = (listSetting(allSettings, "extraCSS") ++ listSetting(allSettings, "extraJS")).flatten.toList,
    layout = stringSetting(allSettings, "layout"),
    configOffset = config.size
  )
}
