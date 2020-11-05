package dotty.dokka
package site

import java.io.File

import org.jetbrains.dokka.base.parsers.MarkdownParser
import org.jetbrains.dokka.base.transformers.pages.comments.DocTagToContentConverter
import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.doc.{DocTag, Text}
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.pages.{ContentKind, ContentNode, DCI, PageNode}
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.pages.Style
import org.jetbrains.dokka.model.DisplaySourceSet

import scala.collection.JavaConverters._

class StaticSiteContext(val root: File, sourceSets: Set[SourceSetWrapper]):

  def indexPage():Option[StaticPageNode] =
    val files = List(new File(root, "index.html"), new File(root, "index.md")).filter { _.exists() }
    if (files.size > 1) println(s"ERROR: Multiple root index pages found: ${files.map(_.getAbsolutePath)}") // TODO (#14): provide proper error handling
    loadFiles(files).headOption

  lazy val layouts: Map[String, TemplateFile] =
    val layoutRoot = new File(root, "_layouts")
    val dirs: Array[File] = Option(layoutRoot.listFiles()).getOrElse(Array())
    dirs.map { it => loadTemplateFile(it) }.map { it => it.name() -> it }.toMap

  private def isValidTemplate(file: File): Boolean =
    (file.isDirectory && !file.getName.startsWith("_")) ||
      file.getName.endsWith(".md") ||
      file.getName.endsWith(".html")


  private def loadTemplate(from: File, isBlog: Boolean = false): Option[LoadedTemplate] =
    if (!isValidTemplate(from)) None else
      try
        val topLevelFiles = if isBlog then Seq(from, new File(from, "_posts")) else Seq(from)
        val allFiles = topLevelFiles.filter(_.isDirectory).flatMap(_.listFiles()) 
        val (indexes, children) = allFiles.flatMap(loadTemplate(_)).partition(_.templateFile.isIndexPage())
        if (indexes.size > 1)
          println(s"ERROR: Multiple index pages for $from found in ${indexes.map(_.file)}") // TODO (#14): provide proper error handling

        def loadIndexPage(): TemplateFile =
          val indexFiles = from.listFiles { file =>file.getName == "index.md" || file.getName == "index.html" }
          indexFiles.size match
            case 0 => emptyTemplate(from)
            case 1 => loadTemplateFile(indexFiles.head).copy(file = from)
            case _ =>
              val msg = s"ERROR: Multiple index pages found under ${from.toPath}"
              throw new java.lang.RuntimeException(msg)

        val templateFile = if (from.isDirectory) loadIndexPage() else loadTemplateFile(from)

        Some(LoadedTemplate(templateFile, children.toList, from))
      catch
          case e: RuntimeException =>
            e.printStackTrace() // TODO (#14): provide proper error handling
            None

  def asContent(doctag: DocTag, dri: DRI) = new DocTagToContentConverter().buildContent(
    doctag,
    new DCI(Set(dri).asJava, ContentKind.Empty),
    sourceSets.asJava,
    JSet(),
    new PropertyContainer(JMap())
  )

  def loadAllFiles() = 
    def dir(name: String)= List(new File(root, name)).filter(_.isDirectory)
    loadFiles(dir("docs").flatMap(_.listFiles())) ++ loadFiles(dir("blog"), isBlog = true)

  def loadFiles(files: List[File], isBlog: Boolean = false): List[StaticPageNode] =
    val all = files.flatMap(loadTemplate(_, isBlog))
    def flatten(it: LoadedTemplate): List[String] =
      List(it.relativePath(root)) ++ it.children.flatMap(flatten)

    def pathToDRI(path: String) = mkDRI(s"_.$path")

    val driMap = all.flatMap(flatten).map(it => it -> pathToDRI(it)).toMap

    def templateToPage(myTemplate: LoadedTemplate): StaticPageNode =
      val dri = pathToDRI(myTemplate.relativePath(root))
      val content = new PartiallyRenderedContent(
        myTemplate.templateFile,
        this,
        JList(),
        new DCI(Set(dri).asJava, ContentKind.Empty),
        sourceSets.toDisplay,
        JSet()
      )
      StaticPageNode(
        myTemplate.templateFile,
        myTemplate.templateFile.title(),
        content,
        JSet(dri),
        JList(),
        (myTemplate.children.map(templateToPage)).asJava
      )

    all.map(templateToPage)
