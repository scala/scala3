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
      "styles/scala3doc-searchbar.css",
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
      "scripts/searchbar.js"
    ).map(dottyRes)

    val urls = List(
      "https://code.jquery.com/jquery-3.5.1.min.js",
      "https://d3js.org/d3.v6.min.js",
      "https://cdn.jsdelivr.net/npm/graphlib-dot@0.6.2/dist/graphlib-dot.min.js",
      "https://cdnjs.cloudflare.com/ajax/libs/dagre-d3/0.6.1/dagre-d3.min.js",
    ).map(Resource.URL.apply)

    fromResources ++ urls ++ projectLogo ++ Seq(scala3docVersionFile, dynamicJsData)

  val searchDataPath = "scripts/searchData.js"
  val memberResourcesPaths = Seq(searchDataPath) ++ memberResources.map(_.path)

  case class PageEntry(
    dri: DRI,
    name: String,
    text: String,
    descr: String,
  ):
    // for jackson
    def getL: String = absolutePath(dri)
    def getN: String = name
    def getT: String = text
    def getD: String = descr

  def searchData(pages: Seq[Page]) =
    def flattenToText(signature: Signature): String =
      signature.map {
        case Link(name, dri) => name
        case s: String => s
      }.mkString

    def processPage(page: Page): Seq[PageEntry] =
      val res =  page.content match
        case m: Member =>
          val descr = m.dri.location.replace("/", ".")
          def processMember(member: Member): Seq[PageEntry] =
            val signatureBuilder = ScalaSignatureProvider.rawSignature(member, InlineSignatureBuilder()).asInstanceOf[InlineSignatureBuilder]
            val sig = Signature(member.kind.name, " ") ++ Seq(Link(member.name, member.dri)) ++ signatureBuilder.names.reverse
            val entry = PageEntry(member.dri, member.name, flattenToText(sig), descr)
            val children = member
                .membersBy(m => m.kind != dotty.dokka.model.api.Kind.Package && !m.kind.isInstanceOf[Classlike])
                .filter(m => m.origin == Origin.RegularlyDefined && m.inheritedFrom.isEmpty)
            Seq(entry) ++ children.flatMap(processMember)

          processMember(m)
        case _ =>
          Seq(PageEntry(page.link.dri, page.link.name, page.link.name, ""))

      res ++ page.children.flatMap(processPage)

    val entries = pages.flatMap(processPage).toArray
    val entriesText = new ObjectMapper().writeValueAsString(entries)
    Resource.Text(searchDataPath, s"pages = $entriesText;")


  def allResources(pages: Seq[Page]): Seq[Resource] = memberResources ++ Seq(
    dottyRes("favicon.ico"),
    dottyRes("fonts/dotty-icons.woff"),
    dottyRes("fonts/dotty-icons.ttf"),
    dottyRes("images/scala3doc_logo.svg"),
    searchData(pages)
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
