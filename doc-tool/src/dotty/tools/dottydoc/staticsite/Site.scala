package dotty.tools
package dottydoc
package staticsite

import java.nio.file.{ Files, FileSystems }
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.io.{ File => JFile }
import java.util.{ List => JList }
import java.nio.file.Path
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import dotc.config.Printers.dottydoc
import dotc.core.Contexts.Context
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Site(val root: JFile, val docs: JList[_]) extends ResourceFinder {
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

  /** Generate HTML files from markdown and .html sources */
  def generateHtmlFiles(outDir: JFile = new JFile(root.getAbsolutePath + "/_site"))(implicit ctx: Context): this.type = {
    if (!outDir.isDirectory) outDir.mkdirs()
    if (!outDir.isDirectory) /*dottydoc.*/println(s"couldn't create output folder: $outDir")
    else compilableFiles.foreach { asset =>
      import scala.collection.JavaConverters._
      val baseUrl: String = {
        val rootLen = root.getAbsolutePath.split('/').length
        val assetLen = asset.getAbsolutePath.split('/').length
        "../" * (assetLen - rootLen - 1) + "."
      }
      val defaultParams = Map(
        "docs" -> docs,
        "page" -> Map("url" -> stripRoot(asset)),
        "site" -> Map("baseUrl" -> baseUrl).asJava
      )
      val fileContents = Source.fromFile(asset).mkString
      val page =
        if (asset.getName.endsWith(".md")) new MarkdownPage(fileContents, defaultParams, includes)
        else new HtmlPage(fileContents, defaultParams, includes)

      val renderedPage = render(page)
      val source = new ByteArrayInputStream(renderedPage.getBytes(StandardCharsets.UTF_8))
      val target = {
        val tgt = stripRoot(asset)
        tgt.splitAt(tgt.lastIndexOf('.'))._1 + ".html"
      }
      val htmlTarget = mkdirs(fs.getPath(outDir.getAbsolutePath, target))
      Files.copy(source, htmlTarget, REPLACE_EXISTING)
    }
    this
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

  // Initialization of `staticAssets` and `compilableAssets`:
  private[this] var _staticAssets: Array[JFile] = _
  private[this] var _compilableFiles: Array[JFile] = _
  private[this] def initFiles() = {
    // Split files between compilable and static assets
    def splitFiles(f: JFile, assets: ArrayBuffer[JFile], comp: ArrayBuffer[JFile]): Unit = {
      val name = f.getName
      if (f.isDirectory) {
        if (!f.getName.startsWith("_")) f.listFiles.foreach(splitFiles(_, assets, comp))
      }
      else if (name.endsWith(".md") || name.endsWith(".html")) comp.append(f)
      else assets.append(f)
    }

    val assets = new ArrayBuffer[JFile]
    val comp = new ArrayBuffer[JFile]
    splitFiles(root, assets, comp)
    _staticAssets = assets.toArray
    _compilableFiles = comp.toArray
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
      "toc.html" -> "/_includes/toc.html"
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
    page.yaml.get("layout").flatMap(layouts.get(_)) match {
      case None =>
        page.html
      case Some(layout) =>
        import scala.collection.JavaConverters._
        val newParams = page.params ++ params ++ Map("page" -> page.yaml) ++ Map("content" -> page.html)
        val expandedTemplate = new HtmlPage(layout, newParams, includes)
        render(expandedTemplate, params)
    }
}
