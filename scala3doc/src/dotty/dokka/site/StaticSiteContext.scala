package dotty.dokka
package site

import java.io.File
import java.nio.file.Files
import java.nio.file.FileVisitOption
import java.nio.file.Path
import java.nio.file.Paths

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
import util.Try

import scala.collection.JavaConverters._

class StaticSiteContext(val root: File, sourceSets: Set[SourceSetWrapper]):

  def indexPage():Option[StaticPageNode] =
    val files = List(new File(root, "index.html"), new File(root, "index.md")).filter { _.exists() }
    // TODO (https://github.com/lampepfl/scala3doc/issues/238): provide proper error handling
    if (files.size > 1) println(s"ERROR: Multiple root index pages found: ${files.map(_.getAbsolutePath)}")
    files.flatMap(loadTemplate(_, isBlog = false)).headOption.map(templateToPage)

  lazy val layouts: Map[String, TemplateFile] =
    val layoutRoot = new File(root, "_layouts")
    val dirs: Array[File] = Option(layoutRoot.listFiles()).getOrElse(Array())
    dirs.map { it => loadTemplateFile(it) }.map { it => it.name -> it }.toMap

  lazy val sideBarConfig =
    val sidebarFile = root.toPath.resolve("sidebar.yml")
    if (!Files.exists(sidebarFile)) None
    else Some(Sidebar.load(Files.readAllLines(sidebarFile).asScala.mkString("\n")))

  lazy val templates: Seq[LoadedTemplate] = sideBarConfig.fold(loadAllFiles())(_.map(loadSidebarContent))

  lazy val mainPages: Seq[StaticPageNode] = templates.map(templateToPage)

  lazy val allPages: Seq[StaticPageNode] = sideBarConfig.fold(mainPages){ sidebar =>
    def flattenPages(p: StaticPageNode): Set[Path] =
      Set(p.template.file.toPath) ++ p.getChildren.asScala.collect { case p: StaticPageNode => flattenPages(p) }.flatten

    val mainFiles = mainPages.toSet.flatMap(flattenPages)
    val docsPath = root.toPath.resolve("docs")
    val allPaths =
      if !Files.exists(docsPath) then Nil
      else Files.walk(docsPath, FileVisitOption.FOLLOW_LINKS).iterator().asScala.toList

    val orphanedFiles = allPaths.filterNot(mainFiles.contains).filter { p =>
        val name = p.getFileName.toString
        name.endsWith(".md") || name.endsWith(".html")
    }

    val orphanedTemplates = orphanedFiles.flatMap(p => loadTemplate(p.toFile, isBlog = false))

    mainPages ++ orphanedTemplates.map(templateToPage)
  }

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
          // TODO (https://github.com/lampepfl/scala3doc/issues/238): provide proper error handling
          println(s"ERROR: Multiple index pages for $from found in ${indexes.map(_.file)}")

        def loadIndexPage(): TemplateFile =
          val indexFiles = from.listFiles { file =>file.getName == "index.md" || file.getName == "index.html" }
          indexFiles.size match
            case 0 => emptyTemplate(from, from.getName)
            case 1 => loadTemplateFile(indexFiles.head).copy(file = from)
            case _ =>
              val msg = s"ERROR: Multiple index pages found under ${from.toPath}"
              throw new java.lang.RuntimeException(msg)

        val templateFile = if (from.isDirectory) loadIndexPage() else loadTemplateFile(from)

        Some(LoadedTemplate(templateFile, children.toList, from))
      catch
          case e: RuntimeException =>
            // TODO (https://github.com/lampepfl/scala3doc/issues/238): provide proper error handling
            e.printStackTrace()
            None

  def asContent(doctag: DocTag, dri: DRI) = new DocTagToContentConverter().buildContent(
    doctag,
    new DCI(Set(dri).asJava, ContentKind.Empty),
    sourceSets.asJava,
    JSet(),
    new PropertyContainer(JMap())
  )

  private def loadSidebarContent(entry: Sidebar): LoadedTemplate = entry match
    case Sidebar.Page(title, url) =>
      val isBlog = title == "Blog"
      val path = if isBlog then "blog" else url.stripSuffix(".html") + ".md"
      val file = root.toPath.resolve(path) // Add support for .html files!
      val LoadedTemplate(template, children, tFile) = loadTemplate(file.toFile, isBlog).get // Add proper logging if file does not exisits
      LoadedTemplate(template.copy(settings = template.settings + ("title" -> title)), children, tFile) 
    case Sidebar.Category(title, nested) =>
      // Add support for index.html/index.md files!
      val fakeFile = new File(root, title)
      LoadedTemplate(emptyTemplate(fakeFile, title), nested.map(loadSidebarContent), fakeFile)

  private def loadAllFiles() =
    def dir(name: String)= List(new File(root, name)).filter(_.isDirectory)
    dir("docs").flatMap(_.listFiles()).flatMap(loadTemplate(_, isBlog = false))
      ++ dir("blog").flatMap(loadTemplate(_, isBlog = true))

  def driForLink(template: TemplateFile, link: String): Try[DRI] = Try(driFor(
        if link.startsWith("/") then root.toPath.resolve(link.drop(1))
        else template.file.toPath.getParent().resolve(link)
    ))

  private def driFor(dest: Path): DRI = mkDRI(s"_.${root.toPath.relativize(dest)}")

  def templateToPage(myTemplate: LoadedTemplate): StaticPageNode =
    val dri = driFor(myTemplate.file.toPath)
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
      myTemplate.templateFile.title,
      content,
      JSet(dri),
      JList(),
      (myTemplate.children.map(templateToPage)).asJava
    )
