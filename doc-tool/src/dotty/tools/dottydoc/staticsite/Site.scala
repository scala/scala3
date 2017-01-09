package dotty.tools
package dottydoc
package staticsite

import _root_.java.io.{ File => JFile }
import dotc.config.Printers.dottydoc
import dotc.core.Contexts.Context
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

class Site(val root: JFile) extends ResourceFinder {
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
      if (f.isDirectory) f.listFiles.foreach(splitFiles(_, assets, comp))
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

    val defaultLayouts: Map[String, String] = Map(
      "main" -> "/_layouts/main.html",
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
      "header.html" -> "/_includes/header.html"
    ).mapValues(getResource)

    defaultIncludes ++ userDefinedIncludes
  }

  private def collectFiles(dir: JFile, includes: String => Boolean): Map[String, String] =
    dir
    .listFiles
    .filter(f => includes(f.getName))
    .map { f =>
      (f.getName.substring(0, f.getName.lastIndexOf('.')), Source.fromFile(f).mkString)
    }
    .toMap

  /** Render a page to html, the resulting string is the result of the complete
    * expansion of the template with all its layouts and includes.
    */
  def render(page: Page, params: Map[String, AnyRef])(implicit ctx: Context): String = {
    page.yaml.get("layout").flatMap(layouts.get(_)) match {
      case None =>
        page.html
      case Some(layout) =>
        val newParams = Map("content" -> page.html) ++ params ++ Map("page" -> page.yaml)
        val expandedTemplate = new HtmlPage(layout, newParams, includes)
        render(expandedTemplate, params)
    }
  }
}
