package dotty.tools
package dottydoc
package staticsite

import java.nio.file.{ Files, FileSystems }
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.io.{ File => JFile }
import java.util.{ List => JList, Map => JMap, Arrays }
import java.nio.file.Path
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import com.vladsch.flexmark.parser.ParserEmulationFamily
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.ext.gfm.tables.TablesExtension
import com.vladsch.flexmark.ext.gfm.strikethrough.StrikethroughExtension
import com.vladsch.flexmark.ext.gfm.tasklist.TaskListExtension
import com.vladsch.flexmark.ext.emoji.EmojiExtension
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.front.matter.YamlFrontMatterExtension
import com.vladsch.flexmark.util.options.{ DataHolder, MutableDataSet }

import dotc.config.Printers.dottydoc
import dotc.core.Contexts.Context
import model.Package
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Site(val root: JFile, val documentation: Map[String, Package]) extends ResourceFinder {
  /** Documentation serialized to java maps */
  private val docs: JList[_] = {
    import model.java._
    documentation.toJavaList
  }

  /** All files that are considered static in this context, this can be
    * anything from CSS, JS to images and other files.
    *
    * @note files that are *not* considered static are files ending in a compilable
    *       extension.
    */
  def staticAssets: Array[JFile] = {
    if (_staticAssets eq null) initFiles()
    _staticAssets
  }

  /** All files that are considered compilable assets in this context. This
    * is mainly markdown and html files, but could include other files in the
    * future.
    *
    * @note files that are considered compilable end in `.md` or `.html`
    */
  def compilableFiles: Array[JFile] = {
    if (_compilableFiles eq null) initFiles()
    _compilableFiles
  }

  /** All files that are considered blogposts, currently this means that files have been placed in:
    *
    * ```
    * ./blog/_posts/year-month-day-title.ext
    * ```
    *
    * where `ext` is either markdown or html.
    */
  def blogposts: Array[JFile] = {
    if (_blogposts eq null) initFiles()
    _blogposts
  }

  protected lazy val blogInfo: Array[BlogPost] =
    blogposts
    .map { file =>
      val BlogPost.extract(year, month, day, name, ext) = file.getName
      val fileContents = Source.fromFile(file).mkString
      val params = defaultParams(file, 2).withUrl(s"/blog/$year/$month/$day/$name.html").toMap
      val page =
        if (ext == "md") new MarkdownPage(fileContents, params, includes)
        else new HtmlPage(fileContents, params, includes)
      BlogPost(file, page)
    }
    .sortBy(_.date)
    .reverse

  // FileSystem getter
  private[this] val fs = FileSystems.getDefault

  def copyStaticFiles(outDir: JFile = new JFile(root.getAbsolutePath + "/_site")): this.type = {
    if (!outDir.isDirectory) outDir.mkdirs()
    if (!outDir.isDirectory) /*dottydoc.*/println(s"couldn't create output folder: $outDir")
    else {
      // Copy user-defined static assets
      staticAssets.foreach { asset =>
        val target = mkdirs(fs.getPath(outDir.getAbsolutePath, stripRoot(asset)))
        val source = mkdirs(fs.getPath(asset.getAbsolutePath))
        Files.copy(source, target, REPLACE_EXISTING)
      }

      // Copy statics included in resources
      Map(
        "css/api-page.css" -> "/css/api-page.css",
        "css/dottydoc.css" -> "/css/dottydoc.css",
        "css/color-brewer.css" -> "/css/color-brewer.css",
        "js/highlight.pack.js" -> "/js/highlight.pack.js"
      )
      .mapValues(getResource)
      .foreach { case (path, resource) =>
        val source = new ByteArrayInputStream(resource.getBytes(StandardCharsets.UTF_8))
        val target = mkdirs(fs.getPath(outDir.getAbsolutePath, path))
        Files.copy(source, target, REPLACE_EXISTING)
      }
    }
    this
  }

  private def defaultParams(pageLocation: JFile, additionalDepth: Int = 0): DefaultParams = {
    import scala.collection.JavaConverters._
    val pathFromRoot = stripRoot(pageLocation)
    val baseUrl: String = {
      val rootLen = root.getAbsolutePath.split('/').length
      val assetLen = pageLocation.getAbsolutePath.split('/').length
      "../" * (assetLen - rootLen - 1 + additionalDepth) + "."
    }

    DefaultParams(docs, PageInfo(pathFromRoot), SiteInfo(baseUrl, Array()))
  }

  private def createOutput(outDir: JFile)(op: => Unit): this.type = {
    if (!outDir.isDirectory) outDir.mkdirs()
    if (!outDir.isDirectory) /*dottydoc.*/println(s"couldn't create output folder: $outDir")
    else op
    this
  }

  /** Generate HTML for the API documentation */
  def generateApiDocs(outDir: JFile = new JFile(root.getAbsolutePath + "/_site"))(implicit ctx: Context): this.type =
    createOutput(outDir) {
      def genDoc(e: model.Entity) = {
        // Suffix is index.html for packages and therefore the additional depth
        // is increased by 1
        val (suffix, offset) =
          if (e.kind == "package") ("/index.html", -1)
          else (".html", 0)

        val target = mkdirs(fs.getPath(outDir.getAbsolutePath +  "/api/" + e.path.mkString("/") + suffix))
        val params = defaultParams(target.toFile, -1).withPosts(blogInfo).withEntity(e)
        val page = new HtmlPage(layouts("api-page"), params.toMap, includes)

        val rendered = render(page)
        val source = new ByteArrayInputStream(rendered.getBytes(StandardCharsets.UTF_8))

        Files.copy(source, target, REPLACE_EXISTING)
      }

      documentation.values.foreach { pkg =>
        genDoc(pkg)
        pkg.members.filterNot(_.kind == "package").map(genDoc)
      }
    }

  /** Generate HTML files from markdown and .html sources */
  def generateHtmlFiles(outDir: JFile = new JFile(root.getAbsolutePath + "/_site"))(implicit ctx: Context): this.type =
    createOutput(outDir) {
      compilableFiles.foreach { asset =>
        val pathFromRoot = stripRoot(asset)
        val fileContents = Source.fromFile(asset).mkString
        val params = defaultParams(asset).withPosts(blogInfo).toMap
        val page =
          if (asset.getName.endsWith(".md")) new MarkdownPage(fileContents, params, includes)
          else new HtmlPage(fileContents, params, includes)

        val renderedPage = render(page)
        val source = new ByteArrayInputStream(renderedPage.getBytes(StandardCharsets.UTF_8))
        val target = pathFromRoot.splitAt(pathFromRoot.lastIndexOf('.'))._1 + ".html"
        val htmlTarget = mkdirs(fs.getPath(outDir.getAbsolutePath, target))
        Files.copy(source, htmlTarget, REPLACE_EXISTING)
      }
    }

  def generateBlog(outDir: JFile = new JFile(root.getAbsolutePath + "/_site"))(implicit ctx: Context): this.type =
    createOutput(outDir) {
      blogposts.foreach { file =>
        val BlogPost.extract(year, month, day, name, ext) = file.getName
        val fileContents = Source.fromFile(file).mkString
        val date = s"$year-$month-$day 00:00:00"
        val params = defaultParams(file, 2).withPosts(blogInfo).withDate(date).toMap
        val page =
          if (ext == "md") new MarkdownPage(fileContents, params, includes)
          else new HtmlPage(fileContents, params, includes)


        val source = new ByteArrayInputStream(render(page).getBytes(StandardCharsets.UTF_8))
        val target = mkdirs(fs.getPath(outDir.getAbsolutePath, "blog", year, month, day, name + ".html"))
        Files.copy(source, target, REPLACE_EXISTING)
      }
    }

  private def mkdirs(path: Path): path.type = {
    val parent = path.getParent.toFile

    if (!parent.isDirectory && !parent.mkdirs())
      dottydoc.println(s"couldn't create directory: $parent")

    path
  }

  /** This function allows the stripping of the path that leads up to root.
    *
    * ```scala
    * stripRoot(new JFile("/some/root/dir/css/index.css"))
    * // returns: dir/css/index.css
    * // given that root is: /some/root
    * ```
    */
  def stripRoot(f: JFile): String = {
    val rootLen = root.getAbsolutePath.length + 1
    f.getAbsolutePath.drop(rootLen)
  }

  // Initialization of `staticAssets` and `compilableAssets`, and `blogPosts`:
  private[this] var _staticAssets: Array[JFile] = _
  private[this] var _compilableFiles: Array[JFile] = _
  private[this] var _blogposts: Array[JFile] = _

  private[this] def initFiles() = {
    // Split files between compilable and static assets
    def splitFiles(f: JFile, assets: ArrayBuffer[JFile], comp: ArrayBuffer[JFile]): Unit = {
      val name = f.getName
      if (f.isDirectory) {
        val name = f.getName
        if (!name.startsWith("_") && name != "api") f.listFiles.foreach(splitFiles(_, assets, comp))
        if (f.getName == "api") dottydoc.println {
          "the specified `/api` directory will not be used since it is needed for the api documentation"
        }
      }
      else if (name.endsWith(".md") || name.endsWith(".html")) comp.append(f)
      else assets.append(f)
    }

    // Collect posts from ./blog/_posts
    def collectPosts(file: JFile): Option[JFile] = file.getName match {
      case BlogPost.extract(year, month, day, name, ext) => Some(file)
      case _ => None
    }

    val assets = new ArrayBuffer[JFile]
    val comp = new ArrayBuffer[JFile]
    splitFiles(root, assets, comp)
    _staticAssets = assets.toArray
    _compilableFiles = comp.toArray

    _blogposts =
      root
      .listFiles
      .find(dir => dir.getName == "blog" && dir.isDirectory)
      .map(_.listFiles).getOrElse(Array.empty)
      .find(dir => dir.getName == "_posts" && dir.isDirectory)
      .map(_.listFiles).getOrElse(Array.empty)
      .flatMap(collectPosts)
  }

  /** Files that define a layout then referred to by `layout: filename-no-ext`
    * in yaml front-matter.
    *
    * The compiler will look in two locations, `<root>/_layouts/` and
    * in the bundled jar file's resources `/_layouts`.
    *
    * If the user supplies a layout that has the same name as one of the
    * defaults, the user-defined one will take precedence.
    */
  val layouts: Map[String, String] = {
    val userDefinedLayouts =
      root
      .listFiles.find(d => d.getName == "_layouts" && d.isDirectory)
      .map(collectFiles(_, f => f.endsWith(".md") || f.endsWith(".html")))
      .getOrElse(Map.empty)
      .map { case (k, v) => (k.substring(0, k.lastIndexOf('.')), v) }

    val defaultLayouts: Map[String, String] = Map(
      "main" -> "/_layouts/main.html",
      "doc" -> "/_layouts/doc.html",
      "doc-page" -> "/_layouts/doc-page.html",
      "api-page" -> "/_layouts/api-page.html",
      "blog" -> "/_layouts/blog.html",
      "index" -> "/_layouts/index.html"
    ).mapValues(getResource)

    defaultLayouts ++ userDefinedLayouts
  }

  /** Include files are allowed under the directory `_includes`. These files
    * have to be compilable files and can be used with liquid includes:
    *
    * ```
    * {% include "some-file" %}
    * ```
    *
    * You can also use the `with` statement:
    *
    * ```
    * {% include "some-file" with { key: value } %}
    * ```
    */
  val includes: Map[String, String] = {
    val userDefinedIncludes =
      root
      .listFiles.find(d => d.getName == "_includes" && d.isDirectory)
      .map(collectFiles(_, f => f.endsWith(".md") || f.endsWith(".html")))
      .getOrElse(Map.empty)

    val defaultIncludes: Map[String, String] = Map(
      "header.html" -> "/_includes/header.html",
      "scala-logo.html" -> "/_includes/scala-logo.html",
      "toc.html" -> "/_includes/toc.html",
      "reference.html" -> "/_includes/reference.html",
      "link.html" -> "/_includes/link.html"
    ).mapValues(getResource)


    defaultIncludes ++ userDefinedIncludes
  }

  private def collectFiles(dir: JFile, includes: String => Boolean): Map[String, String] =
    dir
    .listFiles
    .filter(f => includes(f.getName))
    .map(f => (f.getName, Source.fromFile(f).mkString))
    .toMap

  /** Render a page to html, the resulting string is the result of the complete
    * expansion of the template with all its layouts and includes.
    */
  def render(page: Page, params: Map[String, AnyRef] = Map.empty)(implicit ctx: Context): String =
    page.yaml.get("layout").flatMap(xs => layouts.get(xs.toString)) match {
      case None =>
        page.html
      case Some(layout) =>
        import scala.collection.JavaConverters._
        val newParams = page.params ++ params ++ Map("page" -> page.yaml) ++ Map("content" -> page.html)
        val expandedTemplate = new HtmlPage(layout, newParams, includes)
        render(expandedTemplate, params)
    }
}

object Site {
  val markdownOptions: DataHolder =
    new MutableDataSet()
      .setFrom(ParserEmulationFamily.KRAMDOWN.getOptions)
      .set(Parser.EXTENSIONS, Arrays.asList(
        TablesExtension.create(),
        TaskListExtension.create(),
        AutolinkExtension.create(),
        EmojiExtension.create(),
        YamlFrontMatterExtension.create(),
        StrikethroughExtension.create()
      ))
      .set(EmojiExtension.ROOT_IMAGE_PATH,
        "https://github.global.ssl.fastly.net/images/icons/emoji/")
}
