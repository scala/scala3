package dotty.dokka
package site

import java.io.File

import org.jetbrains.dokka.base.parsers.MarkdownParser
import org.jetbrains.dokka.base.transformers.pages.comments.DocTagToContentConverter
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.doc.{DocTag, Text}
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.pages.{ContentKind, ContentNode, DCI, PageNode}
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.pages.Style

import scala.collection.JavaConverters._

class StaticSiteContext(val root: File, cxt: DokkaContext):

    val docsFile = new File(root, "docs")

    def indexPage():Option[StaticPageNode] = 
      val files = List(new File(root, "index.html"), new File(root, "index.md")).filter { _.exists() }
      if (files.size > 1) println("ERROR: Multiple root index pages found: ${files.map { it.absolutePath }}") // TODO (#14): provide proper error handling
      loadFiles(files).headOption

    private lazy val layouts: Map[String, TemplateFile] =
      val layoutRoot = new File(root, "_layouts")
      val dirs: Array[File] = Option(layoutRoot.listFiles()).getOrElse(Array())
      dirs.map { it => loadTemplateFile(it) }.map { it => it.name() -> it }.toMap

    private def isValidTemplate(file: File): Boolean =
      (file.isDirectory && !file.getName.startsWith("_")) ||
        file.getName.endsWith(".md") ||
        file.getName.endsWith(".html")


    private def loadTemplate(from: File): Option[LoadedTemplate] =
      if (!isValidTemplate(from)) None else 
        try
          val (indexes, children) = Option(from.listFiles()).toSeq.flatten.flatMap(loadTemplate).partition(_.isIndexPage())
          if (indexes.size > 1)
              println("ERROR: Multiple index pages for $from found in ${indexes.map { it.file }}") // TODO (#14): provide proper error handling

          def loadIndexPage(): TemplateFile = {
              val indexFiles = from.listFiles { file =>file.getName == "index.md" || file.getName == "index.html" }
              indexFiles.size match {
                  case 0 => emptyTemplate(from)
                  case 1 => loadTemplateFile(indexFiles.head).copy(file = from)
                  case _ =>  throw new java.lang.RuntimeException("ERROR: Multiple index pages found under ${from.path}")
              }
          }

          val templateFile = if (from.isDirectory) loadIndexPage() else loadTemplateFile(from)

          Some(LoadedTemplate(templateFile, children.toList, from))
        catch
          case e: RuntimeException =>
            e.printStackTrace() // TODO (#14): provide proper error handling
            None

    private def parseMarkdown(page: PreResolvedPage, dri: DRI, allDRIs: Map[String, DRI]): ContentNode =
      val nodes: JList[ContentNode] = if (!page.hasMarkdown) JList() else 
        val parser = new MarkdownParser ( link => {
          val driKey = 
            if (link.startsWith("/")) then
              // handle root related links
              link.replace('/', '.').stripPrefix(".")
            else 
              val unSuffixedDri = dri.getPackageName.stripSuffix(".html").stripSuffix(".md")
              val parentDri = unSuffixedDri.take(unSuffixedDri.lastIndexOf('.')).stripPrefix("_.")
              "${parentDri}.${link.replace('/', '.')}"
          
          allDRIs.get(driKey).orNull
        })
        val docTag = try parser.parseStringToDocNode(page.code) catch
          case (e: Throwable) =>
            val msg = "Error rendering (dri = $dri): ${e.message}"
            println("ERROR: $msg") // TODO (#14): provide proper error handling
            new Text()

        asContent(docTag, dri)

      new PartiallyRenderedContent(
          page,
          nodes,
          new DCI(Set(dri).asJava, ContentKind.Empty),
          cxt.getConfiguration.getSourceSets.toDisplaySourceSet,
          JSet()
      )

    def asContent(d: DocTag, dri: DRI) = new DocTagToContentConverter().buildContent(
        d,
        new DCI(Set(dri).asJava, ContentKind.Empty),
        cxt.getConfiguration.getSourceSets.asDokka,
        JSet(),
        new PropertyContainer(JMap())
    )

    def loadFiles(
        files: List[File],
        customChildren: List[PageNode] = Nil
    ): List[StaticPageNode] =
      val all = files.flatMap(loadTemplate)
      def flatten(it: LoadedTemplate): List[String] =
        List(it.relativePath(root)) ++ it.children.flatMap(flatten)

      def pathTomkDRI(path: String) = mkDRI("_.$path")

      val driMap = all.flatMap { it => flatten(it) }.map { it =>  it -> pathTomkDRI(it) }.toMap

      def templateToPage(myTemplate: LoadedTemplate): StaticPageNode = 
        val dri = pathTomkDRI(myTemplate.relativePath(root))
        val page = try {
          val properties = myTemplate.templateFile.layout()
            .map(c => Map("content" -> myTemplate.templateFile.rawCode)).getOrElse(Map.empty)

          myTemplate.templateFile.resolveMarkdown(RenderingContext(properties, layouts))
        } catch  { case e: Throwable =>
          val msg = "Error rendering $myTemplate: ${e.message}"
          println("ERROR: $msg") // TODO (#14): provide proper error handling
          PreResolvedPage("", None, true)
        }
        val content = parseMarkdown(page, dri, driMap)
        val children = myTemplate.children.map(templateToPage)
        StaticPageNode(
          myTemplate,
          myTemplate.templateFile.title(),
          content,
          JSet(dri),
          JList(),
          (children ++ customChildren).asJava
        )

      all.map(templateToPage)
