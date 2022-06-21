package dotty.tools.scaladoc
package site

import java.io.File
import java.nio.file.{Files, Paths}

import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.emoji.EmojiExtension
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.tables.TablesExtension
import com.vladsch.flexmark.ext.yaml.front.matter.{AbstractYamlFrontMatterVisitor, YamlFrontMatterExtension}
import com.vladsch.flexmark.parser.{Parser, ParserEmulationProfile}
import com.vladsch.flexmark.util.options.{DataHolder, MutableDataSet}
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.formatter.Formatter
import liqp.Template
import liqp.ParseSettings
import liqp.parser.Flavor
import liqp.TemplateContext
import liqp.tags.Tag
import liqp.nodes.LNode
import scala.jdk.CollectionConverters._

import scala.io.Source
import dotty.tools.scaladoc.snippets._
import scala.util.chaining._

/** RenderingContext stores information about defined properties, layouts and sites being resolved
 *
 * @param properties  Map containing defined properties
 * @param layouts     Map containing defined site layouts
 * @param resolving   Set containing names of sites being resolved in this context. This information is useful for cycle detection
 * @param resources   List of resources that need to be appended to sites
 */
case class RenderingContext(
  properties: Map[String, Object],
  layouts: Map[String, TemplateFile] = Map(),
  resolving: Set[String] = Set(),
  resources: List[String] = Nil
):

  def nest(code: String, file: File, resources: List[String]) =
    copy(
      resolving = resolving + file.getAbsolutePath,
      properties = properties + ("content" -> code),
      resources = this.resources ++ resources
    )

case class ResolvedPage(code: String, resources: List[String] = Nil)

enum TemplateName(val name: String):
  case YamlDefined(override val name: String) extends TemplateName(name)
  case SidebarDefined(override val name: String) extends TemplateName(name)
  case FilenameDefined(override val name: String) extends TemplateName(name)

/**
 * case class for the template files.
 * Template file is a file `.md` or `.html` handling settings.
 *
 * @param file     The Actual file defining the template.
 * @param rawCode  The content, what is to be shown, everything but settings.
 * @param settings The config defined in the begging of the file, between the pair of `---` (e.g. layout: basic).
 */
case class TemplateFile(
  file: File,
  isHtml: Boolean,
  rawCode: String,
  settings: Map[String, Object],
  name: String,
  title: TemplateName,
  hasFrame: Boolean,
  resources: List[String],
  layout: Option[String],
  configOffset: Int
):
  def isIndexPage() = file.isFile && (file.getName == "index.md" || file.getName == "index.html")

  private[site] def resolveInner(ctx: RenderingContext)(using ssctx: StaticSiteContext): ResolvedPage =

    lazy val snippetCheckingFunc: SnippetChecker.SnippetCheckingFunc =
      val path = Some(Paths.get(file.getAbsolutePath))
      val pathBasedArg = ssctx.snippetCompilerArgs.get(path)
      val sourceFile = dotty.tools.dotc.util.SourceFile(dotty.tools.io.AbstractFile.getFile(path.get), scala.io.Codec.UTF8)
      (str: String, lineOffset: SnippetChecker.LineOffset, argOverride: Option[SCFlags]) => {
          val arg = argOverride.fold(pathBasedArg)(pathBasedArg.overrideFlag(_))
          val compilerData = SnippetCompilerData(
            "staticsitesnippet",
            SnippetCompilerData.Position(configOffset - 1, 0)
          )
          ssctx.snippetChecker.checkSnippet(str, Some(compilerData), arg, lineOffset, sourceFile).collect {
              case r: SnippetCompilationResult if !r.isSuccessful =>
                r.reportMessages()(using ssctx.outerCtx)
                r
              case r => r
          }
      }

    if (ctx.resolving.contains(file.getAbsolutePath))
      throw new RuntimeException(s"Cycle in templates involving $file: ${ctx.resolving}")

    val layoutTemplate = layout.map(name =>
      ctx.layouts.getOrElse(name, throw new RuntimeException(s"No layouts named $name in ${ctx.layouts}"))
    )

    def asJavaElement(o: Object): Object = o match
      case m: Map[_, _] => m.transform {
        case (k: String, v: Object) => asJavaElement(v)
      }.asJava
      case l: List[_] => l.map(x => asJavaElement(x.asInstanceOf[Object])).asJava
      case other => other

    // Library requires mutable maps..
    val mutableProperties = new JHashMap(ctx.properties.transform((_, v) => asJavaElement(v)).asJava)

    val parseSettings = ParseSettings.Builder().withFlavor(Flavor.JEKYLL).build()

    val rendered = Template.parse(this.rawCode, parseSettings).render(mutableProperties)

    // We want to render markdown only if next template is html
    val code = if (isHtml || layoutTemplate.exists(!_.isHtml)) rendered else
      // Snippet compiler currently supports markdown only
      val parser: Parser = Parser.builder(defaultMarkdownOptions).build()
      val parsedMd = parser.parse(rendered).pipe { md =>
        FlexmarkSnippetProcessor.processSnippets(md, None, snippetCheckingFunc)(using ssctx.outerCtx)
      }.pipe { md =>
        FlexmarkSectionWrapper(md)
      }
      HtmlRenderer.builder(defaultMarkdownOptions).build().render(parsedMd)

    // If we have a layout template, we need to embed rendered content in it. Otherwise, we just leave the content as is.
    layoutTemplate match {
      case Some(t) => t.resolveInner(ctx.nest(code, file, resources))
      case None => ResolvedPage(code, resources ++ ctx.resources)
    }
