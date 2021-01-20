package dotty.dokka
package site

import java.io.File
import java.nio.file.Files
import java.nio.file.FileVisitOption
import java.nio.file.Path
import java.nio.file.Paths

import util.Try
import collection.JavaConverters._

import dotty.dokka.model.api._

class StaticSiteContext(
  val root: File,
  val args: Scala3doc.Args,
  val sourceLinks: SourceLinks)(using val outerCtx: CompilerContext):

  var memberLinkResolver: String => Option[DRI] = _ => None

  def indexTemplate(): Seq[LoadedTemplate] =
    val files = List(new File(root, "index.html"), new File(root, "index.md")).filter { _.exists() }

    if files.size > 1 then
      val msg = s"ERROR: Multiple root index pages found: ${files.map(_.getAbsolutePath)}"
      report.error(msg)

    files.flatMap(loadTemplate(_, isBlog = false)).take(1)

  lazy val layouts: Map[String, TemplateFile] =
    val layoutRoot = new File(root, "_layouts")
    val dirs: Array[File] = Option(layoutRoot.listFiles()).getOrElse(Array())
    dirs.map { it => loadTemplateFile(it) }.map { it => it.name -> it }.toMap

  lazy val sideBarConfig =
    val sidebarFile = root.toPath.resolve("sidebar.yml")
    if (!Files.exists(sidebarFile)) None
    else Some(Sidebar.load(Files.readAllLines(sidebarFile).asScala.mkString("\n")))

  lazy val templates: Seq[LoadedTemplate] = sideBarConfig.fold(loadAllFiles())(_.map(loadSidebarContent))

  lazy val orphanedTemplates: Seq[LoadedTemplate] = {
    def doFlatten(t: LoadedTemplate): Seq[Path] =
      t.file.toPath +: t.children.flatMap(doFlatten)
    val mainFiles = templates.flatMap(doFlatten)

    val allPaths =
      if !Files.exists(docsPath) then Nil
      else Files.walk(docsPath, FileVisitOption.FOLLOW_LINKS).iterator().asScala.toList

    val orphanedFiles = allPaths.filterNot { p =>
       def name = p.getFileName.toString
       def isMain = name == "index.html" || name == "index.md"
       mainFiles.contains(p) || (isMain && mainFiles.contains(p.getParent))
    }.filter { p =>
        val name = p.getFileName.toString
        name.endsWith(".md") || name.endsWith(".html")
    }

    orphanedFiles.flatMap(p => loadTemplate(p.toFile, isBlog = false))
  }

  val docsPath = root.toPath.resolve("docs")

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

        def loadIndexPage(): TemplateFile =
          val indexFiles = from.listFiles { file => file.getName == "index.md" || file.getName == "index.html" }
          indexes match
            case Nil => emptyTemplate(from, from.getName)
            case Seq(loadedTemplate) => loadedTemplate.templateFile.copy(file = from)
            case _ =>
              // TODO (https://github.com/lampepfl/scala3doc/issues/238): provide proper error handling
              val msg = s"ERROR: Multiple index pages for $from found in ${indexes.map(_.file)}"
              throw new java.lang.RuntimeException(msg)

        val templateFile = if (from.isDirectory) loadIndexPage() else loadTemplateFile(from)

        val processedChildren = if !isBlog then children else
          def dateFrom(p: LoadedTemplate): String =
            val pageSettings = p.templateFile.settings.get("page").collect{ case m: Map[String @unchecked, _] => m }
            pageSettings.flatMap(_.get("date").collect{ case s: String => s}).getOrElse("1900-01-01") // blogs without date are last
          children.sortBy(dateFrom).reverse

        val processedTemplate = // Set provided name as arg in page for `docs`
          if from.getParentFile.toPath == docsPath && templateFile.isIndexPage() then
            if templateFile.title != "index" then
              report.warn("Property `title` will be overriden by project name", from)

            templateFile.copy(title = args.name)
          else templateFile

        Some(LoadedTemplate(processedTemplate, processedChildren.toList, from))
      catch
          case e: RuntimeException =>
            // TODO (https://github.com/lampepfl/scala3doc/issues/238): provide proper error handling
            e.printStackTrace()
            None

  private def loadSidebarContent(entry: Sidebar): LoadedTemplate = entry match
    case Sidebar.Page(title, url) =>
      val isBlog = title == "Blog"
      val path = if isBlog then "blog" else
        if Files.exists(root.toPath.resolve(url)) then url
        else url.stripSuffix(".html") + ".md"

      val file = root.toPath.resolve(path).toFile
      val LoadedTemplate(template, children, _) = loadTemplate(file, isBlog).get // Add proper logging if file does not exisits
      LoadedTemplate(template.copy(settings = template.settings + ("title" -> title), file = file), children, file)

    case Sidebar.Category(title, nested) =>
      // Add support for index.html/index.md files!
      val fakeFile = new File(new File(root, "docs"), title)
      LoadedTemplate(emptyTemplate(fakeFile, title), nested.map(loadSidebarContent), fakeFile)

  private def loadAllFiles() =
    def dir(name: String)= List(new File(root, name)).filter(_.isDirectory)
    dir("docs").flatMap(_.listFiles()).flatMap(loadTemplate(_, isBlog = false))
      ++ dir("blog").flatMap(loadTemplate(_, isBlog = true))

  def driForLink(template: TemplateFile, link: String): Seq[DRI] =
    val pathsDri: Option[Seq[DRI]] = Try {
      val baseFile =
        if link.startsWith("/") then root.toPath.resolve(link.drop(1))
        else template.file.toPath.getParent().resolve(link).normalize()

      val baseFileName = baseFile.getFileName.toString
      val mdFile = baseFile.resolveSibling(baseFileName.stripSuffix(".html") + ".md")
      def trySuffix(pref: String) =
       if baseFileName == pref then Seq(baseFile.getParent) else Nil
      val strippedIndexes = trySuffix("index.html") ++ trySuffix("index.md")

      (Seq(baseFile, mdFile) ++ strippedIndexes).filter(Files.exists(_)).map(driFor)
    }.toOption
    pathsDri.getOrElse(memberLinkResolver(link).toList)

  def driFor(dest: Path): DRI =
    val rawFilePath = root.toPath.relativize(dest)
    val pageName = dest.getFileName.toString
    val dotIndex = pageName.lastIndexOf('.')

    val relativePath =
      if rawFilePath.startsWith(Paths.get("blog","_posts")) then
        val regex = raw"(\d*)-(\d*)-(\d*)-(.*)\..*".r
        pageName.toString match
          case regex(year, month, day, name) =>
            rawFilePath.getParent.resolveSibling(Paths.get(year, month, day, name))
          case _ =>
            val msg = s"Relative path for blog: $rawFilePath doesn't match `yyy-mm-dd-name.md` format."
            report.warn(msg, dest.toFile)
            rawFilePath.resolveSibling(pageName.substring(0, dotIndex))
      else
        if (dotIndex < 0) rawFilePath.resolve("index")
        else rawFilePath.resolveSibling(pageName.substring(0, dotIndex))

    DRI.forPath(relativePath)

  def relativePath(myTemplate: LoadedTemplate) = root.toPath.relativize(myTemplate.file.toPath)

  val projectWideProperties =
    Seq("projectName" -> args.name) ++
      args.projectVersion.map("projectVersion" -> _)
