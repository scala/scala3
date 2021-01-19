package dotty.dokka
package renderers

import HTML._
import collection.JavaConverters._
import java.net.URI
import java.net.URL
import dotty.dokka.model.api._
import dotty.dokka.site._
import scala.util.Try
import org.jsoup.Jsoup
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
import java.io.File
import dotty.dokka.translators.FilterAttributes
import com.fasterxml.jackson.databind.ObjectMapper

enum Resource(val path: String):
  case Text(override val path: String, content: String) extends Resource(path)
  case Classpath(override val path: String, name: String) extends Resource(path)
  case File(override val path: String, file: Path) extends Resource(path)
  case URL(url: String) extends Resource(url)

trait Resources(using ctx: DocContext) extends Locations, Writter:
  private def dynamicJsData =
    // If data at any point will become more complex we should use a proper mapping
    val data: Map[String, Map[String, String]] =
      Map("filterDefaults" -> FilterAttributes.defaultValues)
    val str = new ObjectMapper().writeValueAsString(data.transform((_, v) => v.asJava).asJava)
    Resource.Text("scripts/data.js", s"var scala3DocData = $str")

  private def scala3docVersionFile = Resource.Text("scala3doc.version", BuildInfo.version)

  private def projectLogo = ctx.args.projectLogo.toSeq.map { p =>
      val path = Paths.get(p)
      Resource.File(s"project-logo/${path.getFileName()}", path)
  }

  private def dottyRes(path: String) = Resource.Classpath(path, s"dotty_res/$path")

  def linkResources(dri: DRI, resources: Iterable[String]): Iterable[AppliedTag] =
    def fileExtension(url: String): String =
      val param = url.indexOf('?')
      val end = if param < 0 then url.length else param
      val point = url.lastIndexOf('.', end)
      url.substring(point+1, end)

    for res <- resources yield
      fileExtension(res) match
        case "css" => link(rel := "stylesheet", href := resolveLink(dri, res))
        case "js" => script(`type` := "text/javascript", src := resolveLink(dri, res), defer := "true")
        case _ => raw("")

  val memberResources: Seq[Resource] =
    val fromResources = List(
      "styles/nord-light.css",
      "styles/scalastyle.css",
      "styles/dotty-icons.css",
      "styles/diagram.css",
      "styles/filter-bar.css",
      "styles/search-bar.css",
      "hljs/highlight.pack.js",
      "hljs/LICENSE",
      "scripts/hljs-scala3.js",
      "scripts/ux.js",
      "scripts/common/component.js",
      "scripts/common/utils.js",
      "scripts/components/FilterBar.js",
      "scripts/components/DocumentableList.js",
      "scripts/components/Input.js",
      "scripts/components/FilterGroup.js",
      "scripts/components/Filter.js",
    ).map(dottyRes)

    val urls = List(
      "https://code.jquery.com/jquery-3.5.1.min.js",
      "https://d3js.org/d3.v6.min.js",
      "https://cdn.jsdelivr.net/npm/graphlib-dot@0.6.2/dist/graphlib-dot.min.js",
      "https://cdnjs.cloudflare.com/ajax/libs/dagre-d3/0.6.1/dagre-d3.min.js",
    ).map(Resource.URL.apply)

    fromResources ++ urls ++ projectLogo ++ Seq(scala3docVersionFile, dynamicJsData)

  val memberResourcesPaths = memberResources.map(_.path)

  val scala3docLogo = dottyRes("images/scala3doc_logo.svg")

  def packageList(topLevelPackage: Member) = Resource.Text("scala3doc/package-list", "TODO")

  def allResources(topLevelPackage: Member): Seq[Resource] = memberResources ++ Seq(
    dottyRes("favicon.ico"),
    dottyRes("fonts/dotty-icons.woff"),
    dottyRes("fonts/dotty-icons.ttf"),
    scala3docLogo,
    packageList(topLevelPackage)
  )

  def renderResource(resource: Resource): Seq[String] =
    resource match
      case Resource.Text(path, content) =>
        Seq(write(path, content))
      case Resource.Classpath(path, name) =>
        getClass.getClassLoader.getResourceAsStream(name) match
          case null =>
            report.error(s"Unable to find $name on classpath")
            Nil
          case is =>
            try Seq(copy(is, path)) finally is.close()
      case Resource.File(path, file) =>
        Seq(copy(file, path))
      case Resource.URL(url) =>
        Nil
