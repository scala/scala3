package dotty.dokka
package site

import java.io.File
import java.nio.file.Files
import dotty.dokka.model.api._

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

import org.jetbrains.dokka.model.doc.Text
import org.jetbrains.dokka.model.Documentable
import org.jetbrains.dokka.transformers.pages.PageTransformer
import org.jetbrains.dokka.pages._

import scala.collection.JavaConverters._

val docsRootDRI: DRI = DRI(location = "index.md")
val docsDRI: DRI = DRI(location = "docs.index.md")
val apiPageDRI: DRI = DRI(location = "api", extra = "__api__")

val defaultMarkdownOptions: DataHolder =
  new MutableDataSet()
    .setFrom(ParserEmulationProfile.KRAMDOWN.getOptions)
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
      WikiLinkExtension.create()
    ))

def emptyTemplate(file: File, title: String): TemplateFile = TemplateFile(
  file = file,
  isHtml = true,
  rawCode = "",
  settings = Map.empty,
  name = file.getName.stripSuffix(".html"),
  title = title,
  hasFrame = true,
  resources = List.empty,
  layout = None
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

  def getSettingValue(k: String, v: JList[String]): String | List[String] =
    if v.size == 1 then v.get(0) else v.asScala.toList

  val globalKeys = Set("extraJS", "extraCSS", "layout", "hasFrame", "name", "title")
  val allSettings = yamlCollector.getData.asScala.toMap.transform(getSettingValue)
  val (global, inner) = allSettings.partition((k,_) => globalKeys.contains(k))
  val settings = Map("page" -> inner)

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
    title = stringSetting(allSettings, "title").getOrElse(name),
    hasFrame = !stringSetting(allSettings, "hasFrame").contains("false"),
    resources = (listSetting(allSettings, "extraCSS") ++ listSetting(allSettings, "extraJS")).flatten.toList,
    layout = stringSetting(allSettings, "layout")
  )
}

def Text(msg: String = "") = new Text(msg, JList(), JMap())

abstract class BaseStaticSiteProcessor(using ctx: DocContext)
  extends PageTransformer:
    final override def invoke(input: RootPageNode): RootPageNode =
      ctx.staticSiteContext.fold(input)(transform(input, _))

    protected def transform(input: RootPageNode, ctx: StaticSiteContext): RootPageNode

// Needed until we will migrate away from dokka
case class FakeContentPage(
  dri: DRI,
  override val getContent: ContentNode) extends ContentPage:
  override val getName: String = ""
  override val getChildren: JList[PageNode] = JList()
  override val getEmbeddedResources: JList[String] = JList()
  override def getDocumentable: Documentable = null
  override def modified(
    name: String,
    content: ContentNode,
    dri: JSet[DRI],
    embeddedResources: JList[String],
    children: JList[_ <: PageNode]
  ): ContentPage = this
  override def modified(name: String, children: JList[_ <: PageNode]): PageNode = this
  override val getDri: JSet[DRI] = JSet(dri)

case class AContentPage(
  override val getName: String,
  override val getChildren: JList[PageNode],
  override val getContent: ContentNode,
  override val getDri: JSet[DRI],
  override val getEmbeddedResources: JList[String] = JList(),
) extends ContentPage:
  override def getDocumentable: Documentable = null

  override def modified(
    name: String,
    content: ContentNode,
    dri: JSet[DRI],
    embeddedResources: JList[String],
    children: JList[_ <: PageNode]
  ): ContentPage =
    copy(name, children.asInstanceOf[JList[PageNode]], content, dri, embeddedResources)

  override def modified(name: String, children: JList[_ <: PageNode]): PageNode =
    copy(name, getChildren = children.asInstanceOf[JList[PageNode]])
