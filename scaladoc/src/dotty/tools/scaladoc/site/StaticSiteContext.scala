package dotty.tools.scaladoc
package site

import java.io.File
import java.nio.file.Files
import java.nio.file.FileVisitOption
import java.nio.file.Path
import java.nio.file.Paths

import scala.util.control.NonFatal


class StaticSiteContext(
  val root: File,
  val args: Scaladoc.Args,
  val sourceLinks: SourceLinks,
  val snippetCompilerArgs: snippets.SnippetCompilerArgs,
  val snippetChecker: snippets.SnippetChecker)(using val outerCtx: CompilerContext):

  var memberLinkResolver: String => Option[DRI] = _ => None

  val docsPath = root.toPath.resolve("_docs")
  val blogPath = root.toPath.resolve("_blog")

  def resolveNewBlogPath(stringPath: String): Path =
    if stringPath.nonEmpty then root.toPath.resolve(stringPath)
    else blogPath

  def relativize(path: Path): Path =
    if args.apiSubdirectory then
      docsPath.relativize(path)
    else
      val relativised = docsPath.relativize(path)
      Paths.get("docs").resolve(relativised)

  val siteExtensions = Set(".html", ".md")

  lazy val layouts: Map[String, TemplateFile] =
    val layoutRoot = new File(root, "_layouts")
    val dirs: Array[File] = Option(layoutRoot.listFiles()).getOrElse(Array())
    dirs.map { it => loadTemplateFile(it)(using this) }.map { it => it.name -> it }.toMap

  lazy val staticSiteRoot: StaticSiteRoot = StaticSiteLoader(root, args)(using this, outerCtx).load()



  lazy val allTemplates =
    def process(l: LoadedTemplate): List[LoadedTemplate] =
      l +: l.children.flatMap(process)
    process(staticSiteRoot.rootTemplate)



  /** Handles redirecting from multiple locations to one page
   *
   * For each entry in redirectFrom setting, create a page which contains code that redirects you to the page where the redirectFrom is defined.
   */
  lazy val redirectTemplates: Seq[(LoadedTemplate, DRI, DRI)] =
    allTemplates.flatMap { loadedTemplate =>
      val redirectFrom = loadedTemplate.templateFile.settings.getOrElse("page", Map.empty).asInstanceOf[Map[String, Object]].get("redirectFrom")
      def redirectToTemplate(redirectFrom: String) =
        val path = if redirectFrom.startsWith("/")
          then docsPath.resolve(redirectFrom.drop(1))
          else loadedTemplate.file.toPath.resolveSibling(redirectFrom)
        val driFrom = driFor(path)
        val driTo = driFor(loadedTemplate.file.toPath)
        (LoadedTemplate(layouts("redirect"), List.empty, path.toFile), driFrom, driTo)
      redirectFrom.map {
        case redirectFrom: String => Seq(redirectToTemplate(redirectFrom))
        case redirects: List[?] => redirects.asInstanceOf[List[String]].map(redirectToTemplate)
      }.getOrElse(Nil)
    }

  def driForLink(loadedTemplateFile: File, link: String): Seq[DRI] =
    def possibleLinks(link: String): Seq[Path] =
      // Destination file of template
      val templateDestLocation = loadedTemplateFile.toPath
      // Source file of template (may not exist - e.g. redirect templates)
      val templateSourceLocation = staticSiteRoot.reverseSiteMappings.get(templateDestLocation)

      // Check if link is relative or absolute
      try
        if link.startsWith("/")
        then Seq(root.toPath.resolve(link.drop(1)))
        else Seq(templateDestLocation.getParent.resolve(link).normalize) ++
          templateSourceLocation.map(_.getParent.resolve(link).normalize)
      catch
        case NonFatal(_) => Seq.empty

    // Try to strip site extension and create all possible file paths
    val fileNames = if siteExtensions.exists(link.endsWith(_))
      then {
        val withoutExt = siteExtensions.find(link.endsWith(_)).fold(link)(ext => link.stripSuffix(ext))
        siteExtensions.map(withoutExt + _).toSeq
      }
      else Seq(link)

    val baseFiles = fileNames.flatMap(possibleLinks)

    // If file exists in source files, map it to destination file. If file exists in destination file, just take it.
    val links = baseFiles.flatMap(p =>
      Option.when(staticSiteRoot.sources.contains(p))(p).map(p => staticSiteRoot.siteMappings(p))
        .orElse(Option.when(staticSiteRoot.dests.contains(p))(p))
    )

    links match {
      case link :: rest => Seq(driFor(link))
      case Nil => memberLinkResolver(link).toSeq
    }

  def driFor(dest: Path): DRI =
    val rawFilePath = relativize(dest)
    val pageName = dest.getFileName.toString
    val dotIndex = pageName.lastIndexOf('.')

    val relativePath =
      if (dotIndex < 0) rawFilePath.resolve("index")
      else rawFilePath.resolveSibling(pageName.substring(0, dotIndex))

    DRI.forPath(relativePath)

  def pathFromRoot(myTemplate: LoadedTemplate) = root.toPath.relativize(myTemplate.file.toPath)


  val projectWideProperties =
    Seq("projectName" -> args.name) ++
      args.projectVersion.map("projectVersion" -> _)

