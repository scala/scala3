package dotty.tools.scaladoc
package site

import java.io.File
import java.nio.file.Files
import java.nio.file.{ Paths, Path }
import scala.io._

class StaticSiteLoader(val root: File, val args: Scaladoc.Args)(using StaticSiteContext, CompilerContext):
  val ctx: StaticSiteContext = summon[StaticSiteContext]

  val possibleYamlFiles: Seq[String] = Seq(
    "sidebar.yml"
  )

  def load(): StaticSiteRoot = {
    // Check whether there's YAML file defining static site structure
    possibleYamlFiles
      .map(name => root.toPath.resolve(name))
      .find(path => Files.exists(path))
      .fold(loadBasedOnFileSystem()) { path =>
        val entries = Sidebar.load(path.toFile)
        loadBasedOnYaml(entries)
      }
  }

  def loadBasedOnYaml(yamlRoot: Sidebar.Root): StaticSiteRoot = {
    val rootDest = ctx.docsPath.resolve("index.html").toFile
    val rootIndex = yamlRoot.index
      .map(Paths.get(root.getPath, _).toFile)
      .filter(_.exists)
      .fold(emptyTemplate(rootDest, "index")) { f =>
        val loaded = loadTemplateFile(f)
        if loaded.title.name != "index"
        then report.warn("Property `title` will be overridden by project name", f)
        loaded
      }.copy(title = TemplateName.FilenameDefined(args.name))

    def loadChild(pathFromRoot: Path): Sidebar => LoadedTemplate = {
      case Sidebar.Category(optionTitle, optionIndexPath, nested, dir) =>
        val indexPageOpt = optionIndexPath
          .map(relativizeIfNeeded)
          .map(_.toFile)
          .filter(_.exists)
          .map(loadTemplateFile)
        val title = (
          optionTitle.map(TemplateName.SidebarDefined(_)) ++
          indexPageOpt.map(_.title)
        ).headOption.getOrElse {
          report.error(s"Title for subsection needs to be set in YAML config or in index file")
          TemplateName.FilenameDefined("unnamed_section")
        }
        val categoryPath = dir.fold(pathFromRoot.resolve(toKebabCase(title.name)))(pathFromRoot.resolve(_))
        val indexPage = indexPageOpt.getOrElse(emptyTemplate(categoryPath.resolve("index.html").toFile, title.name))

        val children = optionIndexPath.filter(_ => nested.isEmpty).fold(
          nested.map(child => loadChild(categoryPath)(child))
        ) { indexPath =>
          val indexPathDirectory = Paths.get(indexPath).getParent
          val filesInDirectory = Option(root.toPath.resolve(indexPathDirectory).toFile.listFiles)
          filesInDirectory.fold(List.empty) { files =>
            val mappingFunc: File => File = file => {
              val relativeFile = root.toPath.resolve(indexPathDirectory).relativize(file.toPath)
              categoryPath.resolve(relativeFile).toFile
            }
            files.toList
              .filter(_.toPath != indexPage.file.toPath)
              .flatMap(file => loadRecursively(file, mappingFunc))
          }
        }

        LoadedTemplate(indexPage, children, categoryPath.resolve("index.html").toFile)
      case Sidebar.Page(optionTitle, pagePath) =>
        val path = relativizeIfNeeded(pagePath)
        val file = path.toFile
        val templateFile = loadTemplateFile(file)
        val withUpdatedTitle = optionTitle.fold(templateFile) { t => templateFile.title match
          case _: TemplateName.FilenameDefined => templateFile.copy(title = TemplateName.SidebarDefined(t))
          case _ => templateFile
        }
        LoadedTemplate(withUpdatedTitle, List.empty, pathFromRoot.resolve(file.getName).toFile)
      case Sidebar.Root(_, _) =>
        // Cannot happen
        ???
    }
    val rootTemplate = LoadedTemplate(rootIndex, yamlRoot.pages.map(c => loadChild(ctx.docsPath)(c)) ++ loadBlog(), rootDest)
    val mappings = createMapping(rootTemplate)
    StaticSiteRoot(rootTemplate, mappings)
  }

  def loadBasedOnFileSystem(): StaticSiteRoot = {
    val rootTemplate =
      loadRecursively(ctx.docsPath.toFile).getOrElse(
        LoadedTemplate(emptyTemplate(ctx.docsPath.resolve("index.html").toFile, "index"), List.empty, ctx.docsPath.resolve("index.html").toFile)
      )

    if rootTemplate.templateFile.title.name != "index" then {
      report.warn("Property `title` will be overridden by project name", rootTemplate.templateFile.file)
    }

    val withChangedTitle =
      rootTemplate.copy(templateFile = rootTemplate.templateFile.copy(title = TemplateName.FilenameDefined(args.name)))

    val withBlog = loadBlog().fold(withChangedTitle)(blog => withChangedTitle.copy(children = withChangedTitle.children :+ blog))

    val mappings = createMapping(withBlog)
    StaticSiteRoot(withBlog, mappings)
  }

  def loadBlog(): Option[LoadedTemplate] = {
    type Date = (String, String, String)
    val rootPath = ctx.blogPath
    if (!Files.exists(rootPath)) None
    else {
      val indexPageOpt = Seq(
          rootPath.resolve("index.md"),
          rootPath.resolve("index.html"),
      ).filter(p => Files.exists(p)) match {
          case Nil => None
          case indexPath :: Nil => Some(indexPath)
          case list =>
            report.warning(s"Multiple index pages for $rootPath found in ${list.map(_.toFile)}. Defaulting to first.")
            list.headOption
      }
      val indexTemplateOpt = indexPageOpt.map(p => loadTemplateFile(p.toFile))

      val indexPage = indexTemplateOpt.getOrElse(emptyTemplate(rootPath.resolve("index.html").toFile, "Blog"))
      val indexDest = ctx.docsPath.resolve("_blog").resolve("index.html")
      val regex = raw"(\d*)-(\d*)-(\d*)-(.*)".r
      def splitDateName(tf: TemplateFile): (Date, String) = tf.file.getName match
          case regex(year, month, day, name) => ((year, month, day), name)
          case name =>
            report.warn("Incorrect file name for blog post. Post file name should be in format <year>-<month>-<day>-<name>", tf.file)
            (("1900","01","01"), name)

      def dateFrom(tf: TemplateFile, default: String = "1900-01-01"): String =
        val pageSettings = tf.settings.get("page").collect{ case m: Map[String @unchecked, _] => m }
        pageSettings.flatMap(_.get("date").collect{ case s: String => s}).getOrElse(default) // blogs without date are last

      val posts = List(rootPath.resolve("_posts"))
        .filter(Files.exists(_))
        .flatMap(_.toFile.listFiles)
        .filterNot(_.isDirectory)
        .map { postFile =>
          val templateFile = loadTemplateFile(postFile)
          val ((year, month, day), name) = splitDateName(templateFile)
          val destPath = ctx.docsPath.resolve("_blog").resolve(year).resolve(month).resolve(day).resolve(name)
          val date = dateFrom(templateFile, s"$year-$month-$day")
          date -> LoadedTemplate(templateFile, List.empty, destPath.toFile)
        }.sortBy(_._1).reverse.map(_._2)

      Some(LoadedTemplate(indexPage, posts, indexDest.toFile))
    }
  }

  def loadRecursively(currRoot: File, destMappingFunc: File => File = identity): Option[LoadedTemplate] = {
    val rootPath = currRoot.toPath
    if currRoot.isDirectory
    then {
      val indexPageOpt = Seq(
        rootPath.resolve("index.md"),
        rootPath.resolve("index.html"),
        rootPath.resolve(s"${currRoot.getName}.md"),
        rootPath.resolve(s"${currRoot.getName}.html")
      ).filter(p => Files.exists(p)) match {
        case Nil => None
        case indexPath :: Nil => Some(indexPath)
        case list =>
          report.warning(s"Multiple index pages for $currRoot found in ${list.map(_.toFile)}. Defaulting to first.")
          list.headOption
      }
      val templateFileOpt = indexPageOpt.map(p => loadTemplateFile(p.toFile))

      val indexPage = templateFileOpt.getOrElse(emptyTemplate(rootPath.resolve("index.html").toFile, "index"))

      val children = currRoot.listFiles.toList
        .filter(_.toPath != indexPageOpt.getOrElse(null))
      Some(LoadedTemplate(indexPage, children.flatMap(loadRecursively(_, destMappingFunc)), destMappingFunc(indexPage.file)))
    }
    else if (currRoot.exists && ctx.siteExtensions.exists(ext => currRoot.getName.endsWith(ext))) {
      val templateFile = loadTemplateFile(currRoot)
      Some(LoadedTemplate(templateFile, List.empty, destMappingFunc(templateFile.file)))
    }
    else None
  }

  def createMapping(root: LoadedTemplate): Map[Path, Path] =
    Map((root.templateFile.file.toPath, root.file.toPath)) ++ root.children.map(createMapping).fold(Map.empty)(_ ++ _)

  private def toKebabCase(s: String) = s.toLowerCase.replace(' ', '-')

  private def relativizeIfNeeded(link: String): Path =
    val path = Paths.get(link)
    if !path.isAbsolute then ctx.docsPath.resolve(link)
    else path

  extension (p: Path)
    private def toHtml: Path = p.getParent.resolve(p.getFileName.toString match
      case f if f.endsWith(".md") => f.stripSuffix(".md") + ".html"
      case f => f
    )