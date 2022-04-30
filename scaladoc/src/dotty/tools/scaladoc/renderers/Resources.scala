package dotty.tools.scaladoc
package renderers

import util.HTML._
import collection.JavaConverters._
import java.net.URI
import java.net.URL
import dotty.tools.scaladoc.site._
import scala.util.Try
import org.jsoup.Jsoup
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
import java.io.File
import dotty.tools.scaladoc.translators.FilterAttributes
import util._
import translators._

enum Resource(val path: String):
  case Text(override val path: String, content: String) extends Resource(path)
  case Classpath(override val path: String, name: String) extends Resource(path)
  case File(override val path: String, file: Path) extends Resource(path)
  case URL(url: String) extends Resource(url)
  case URLToCopy(url: String, dest: String) extends Resource(url)

trait Resources(using ctx: DocContext) extends Locations, Writer:
  private def dynamicJsData =
    val str = jsonObject("filterDefaults" -> jsonObject(
      FilterAttributes.defaultValues.toSeq.map { case  (n, v) => n -> jsonString(v) }:_*
    ))
    Resource.Text("scripts/data.js", s"var scaladocData = $str")

  private def scaladocVersionFile = Resource.Text("scaladoc.version", BuildInfo.version)

  lazy val projectLogo = ctx.args.projectLogo.map { p =>
      val path = Paths.get(p)
      Resource.File(s"project-logo/${path.getFileName()}", path)
  }

  lazy val darkProjectLogo = ctx.args.projectLogo.map(p => Paths.get(p))
    .map { p =>
      val darkFileName = p.getFileName.toString.split('.').toList match
        case Nil => "logo_dark"
        case oneElem :: Nil => oneElem + "_dark"
        case list =>
          val (init, last) = (list.init, list.last)
          init.mkString(".") + "_dark." + last
      p.resolveSibling(darkFileName)
    }
    .filter(p => Files.exists(p))
    .map { path =>
      Resource.File(s"project-logo/${path.getFileName()}", path)
    }

  private def dottyRes(path: String) = Resource.Classpath(path, s"dotty_res/$path")

  def linkResources(dri: DRI, resources: Iterable[String], deferJs: Boolean): Iterable[AppliedTag] =
    def fileExtension(url: String): String =
      val param = url.indexOf('?')
      val end = if param < 0 then url.length else param
      val point = url.lastIndexOf('.', end)
      url.substring(point+1, end)
    for res <- resources yield
      fileExtension(res) match
        case "css" => link(rel := "stylesheet", href := resolveLink(dri, res))
        case "js" => script(`type` := "text/javascript", src := resolveLink(dri, res), if (deferJs) Seq(defer := "true") else Nil)
        case _ => raw("")

  val onlyRenderedResources: Seq[Resource] =
    List(
      "scripts/inkuire.js"
    ).map(dottyRes) ++
    List(
      "scripts/inkuire-worker.js",
      "webfonts/fa-brands-400.eot",
      "webfonts/fa-brands-400.svg",
      "webfonts/fa-brands-400.ttf",
      "webfonts/fa-brands-400.woff",
      "webfonts/fa-brands-400.woff2",
      "webfonts/fa-regular-400.eot",
      "webfonts/fa-regular-400.svg",
      "webfonts/fa-regular-400.ttf",
      "webfonts/fa-regular-400.woff",
      "webfonts/fa-regular-400.woff2",
      "webfonts/fa-solid-900.eot",
      "webfonts/fa-solid-900.svg",
      "webfonts/fa-solid-900.ttf",
      "webfonts/fa-solid-900.woff",
      "webfonts/fa-solid-900.woff2"
    ).map(dottyRes)


  val earlyCommonResources: Seq[Resource] =
    List(
      "scripts/theme.js"
    ).map(dottyRes)

  val commonResources: Seq[Resource] = {
    val fromResources = List(
      "styles/theme/colors.css",
      "styles/theme/color-tokens.css",
      "styles/theme/spacing.css",
      "styles/theme/typography.css",

      // layout
      "styles/theme/layout/container.css",
      "styles/theme/layout/header.css",
      "styles/theme/layout/leftMenu.css",
      "styles/theme/layout/sideMenu.css",
      "styles/theme/layout/searchBar.css",
      "styles/theme/layout/floatingButton.css",
      "styles/theme/layout/mobileMenu.css",
      "styles/theme/layout/footer.css",

      // components
      "styles/theme/components/switcher.css",
      "styles/theme/components/navigation-item.css",
      "styles/theme/components/button/icon-button.css",
      "styles/theme/components/button/text-button.css",
      "styles/theme/components/dropdown-menu.css",
      "styles/theme/components/divider.css",

      "styles/nord-light.css",
      "styles/dotty-icons.css",
      "styles/diagram.css",
      "styles/filter-bar.css",
      "styles/code-snippets.css",
      "styles/searchbar.css",
      "styles/social-links.css",
      "styles/ux.css",
      "styles/versions-dropdown.css",
      "styles/fontawesome.css",
      "hljs/highlight.min.js",
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
      "scripts/scaladoc-scalajs.js"
    ).map(dottyRes)

    val urls = List(
      "https://code.jquery.com/jquery-3.5.1.min.js",
      "https://d3js.org/d3.v6.min.js",
      "https://cdn.jsdelivr.net/npm/graphlib-dot@0.6.2/dist/graphlib-dot.min.js",
      "https://cdnjs.cloudflare.com/ajax/libs/dagre-d3/0.6.1/dagre-d3.min.js",
      "https://scastie.scala-lang.org/embedded.js"
    ).map(Resource.URL.apply)

    fromResources ++ urls ++ projectLogo ++ darkProjectLogo ++ Seq(scaladocVersionFile, dynamicJsData)
  }

  val apiOnlyResources = List(
    "styles/apistyles.css"
  ).map(dottyRes)

  val staticSiteOnlyResources = List(
    "styles/staticsitestyles.css"
  ).map(dottyRes)

  val searchDataPath = "scripts/searchData.js"
  val scastieConfigurationPath = "scripts/scastieConfiguration.js"
  val commonResourcesPaths = Seq(searchDataPath) ++ Seq(scastieConfigurationPath) ++ commonResources.map(_.path)
  val earlyCommonResourcePaths = earlyCommonResources.map(_.path)

  val apiOnlyResourcesPaths: Seq[String] = apiOnlyResources.map(_.path)
  val staticSiteOnlyResourcesPaths: Seq[String] = staticSiteOnlyResources.map(_.path)

  def searchData(pages: Seq[Page]) =
    def flattenToText(signature: Signature): String =
      signature.map {
        case Type(name, dri) => name
        case Plain(s) => s
        case Keyword(s) => s
      }.mkString

    def mkEntry(dri: DRI, name: String, text: String, descr: String, kind: String) = jsonObject(
        "l" -> jsonString(relativeInternalOrAbsoluteExternalPath(dri)),
        "e" -> (if dri.externalLink.isDefined then rawJSON("true") else rawJSON("false")),
        "n" -> jsonString(name),
        "t" -> jsonString(text),
        "d" -> jsonString(descr),
        "k" -> jsonString(kind)
      )

    def processPage(page: Page): Seq[JSON] =
      val res =  page.content match
        case m: Member if m.kind != Kind.RootPackage =>
          val descr = m.dri.asFileLocation
          def processMember(member: Member): Seq[JSON] =
            val signatureBuilder = ScalaSignatureProvider.rawSignature(member, InlineSignatureBuilder())().asInstanceOf[InlineSignatureBuilder]
            val sig = Signature(Plain(member.name)) ++ signatureBuilder.names.reverse
            val entry = mkEntry(member.dri, member.name, flattenToText(sig), descr, member.kind.name)
            val children = member
                .membersBy(m => m.kind != Kind.Package && !m.kind.isInstanceOf[Classlike])
                .filter(m => m.origin == Origin.RegularlyDefined && m.inheritedFrom.fold(true)(_.isSourceSuperclassHidden))
            Seq(entry) ++ children.flatMap(processMember)

          processMember(m)
        case _ =>
          Seq(mkEntry(page.link.dri, page.link.name, page.link.name, "", "static"))

      res ++ page.children.flatMap(processPage)

    val entries = pages.flatMap(processPage)
    Resource.Text(searchDataPath, s"pages = ${jsonList(entries)};")

  def scastieConfiguration() =
    Resource.Text(scastieConfigurationPath, s"""scastieConfiguration = "${
      ctx.args.scastieConfiguration.replace('"'.toString, """\"""")
    }"""")


  def allResources(pages: Seq[Page]): Seq[Resource] =
    earlyCommonResources ++
    commonResources ++
    apiOnlyResources ++
    staticSiteOnlyResources ++
    Seq(
      dottyRes("favicon.ico"),
      dottyRes("fonts/dotty-icons.woff"),
      dottyRes("fonts/dotty-icons.ttf"),
      dottyRes("fonts/Inter-Bold.ttf"),
      dottyRes("fonts/Inter-Medium.ttf"),
      dottyRes("fonts/Inter-Regular.ttf"),
      dottyRes("fonts/Inter-SemiBold.ttf"),
      dottyRes("fonts/FiraCode-Regular.ttf"),
      dottyRes("images/scaladoc_logo.svg"),
      dottyRes("images/scaladoc_logo_dark.svg"),
      dottyRes("images/class.svg"),
      dottyRes("images/class_comp.svg"),
      dottyRes("images/class-dark.svg"),
      dottyRes("images/object.svg"),
      dottyRes("images/object_comp.svg"),
      dottyRes("images/object-dark.svg"),
      dottyRes("images/trait.svg"),
      dottyRes("images/trait-dark.svg"),
      dottyRes("images/trait_comp.svg"),
      dottyRes("images/enum.svg"),
      dottyRes("images/enum-dark.svg"),
      dottyRes("images/enum_comp.svg"),
      dottyRes("images/given.svg"),
      dottyRes("images/given-dark.svg"),
      dottyRes("images/method.svg"),
      dottyRes("images/method-dark.svg"),
      dottyRes("images/type.svg"),
      dottyRes("images/type-dark.svg"),
      dottyRes("images/val.svg"),
      dottyRes("images/val-dark.svg"),
      dottyRes("images/package.svg"),
      dottyRes("images/package-dark.svg"),
      dottyRes("images/static.svg"),
      dottyRes("images/inkuire.svg"),
      dottyRes("images/static-dark.svg"),
      dottyRes("images/github-icon-black.png"),
      dottyRes("images/github-icon-white.png"),
      dottyRes("images/discord-icon-black.png"),
      dottyRes("images/discord-icon-white.png"),
      dottyRes("images/twitter-icon-black.png"),
      dottyRes("images/twitter-icon-white.png"),
      dottyRes("images/gitter-icon-black.png"),
      dottyRes("images/gitter-icon-white.png"),
      dottyRes("images/icon-buttons/sun/dark/active.svg"),
      dottyRes("images/icon-buttons/sun/dark/disabled.svg"),
      dottyRes("images/icon-buttons/sun/dark/focus.svg"),
      dottyRes("images/icon-buttons/sun/dark/hover.svg"),
      dottyRes("images/icon-buttons/sun/dark/selected.svg"),
      dottyRes("images/icon-buttons/sun/dark/default.svg"),
      dottyRes("images/icon-buttons/sun/light/active.svg"),
      dottyRes("images/icon-buttons/sun/light/disabled.svg"),
      dottyRes("images/icon-buttons/sun/light/focus.svg"),
      dottyRes("images/icon-buttons/sun/light/hover.svg"),
      dottyRes("images/icon-buttons/sun/light/selected.svg"),
      dottyRes("images/icon-buttons/sun/light/default.svg"),
      dottyRes("images/icon-buttons/hamburger/dark/active.svg"),
      dottyRes("images/icon-buttons/hamburger/dark/disabled.svg"),
      dottyRes("images/icon-buttons/hamburger/dark/focus.svg"),
      dottyRes("images/icon-buttons/hamburger/dark/hover.svg"),
      dottyRes("images/icon-buttons/hamburger/dark/selected.svg"),
      dottyRes("images/icon-buttons/hamburger/dark/default.svg"),
      dottyRes("images/icon-buttons/hamburger/light/active.svg"),
      dottyRes("images/icon-buttons/hamburger/light/disabled.svg"),
      dottyRes("images/icon-buttons/hamburger/light/focus.svg"),
      dottyRes("images/icon-buttons/hamburger/light/hover.svg"),
      dottyRes("images/icon-buttons/hamburger/light/selected.svg"),
      dottyRes("images/icon-buttons/hamburger/light/default.svg"),
      dottyRes("images/icon-buttons/moon/dark/active.svg"),
      dottyRes("images/icon-buttons/moon/dark/disabled.svg"),
      dottyRes("images/icon-buttons/moon/dark/focus.svg"),
      dottyRes("images/icon-buttons/moon/dark/hover.svg"),
      dottyRes("images/icon-buttons/moon/dark/selected.svg"),
      dottyRes("images/icon-buttons/moon/dark/default.svg"),
      dottyRes("images/icon-buttons/moon/light/active.svg"),
      dottyRes("images/icon-buttons/moon/light/disabled.svg"),
      dottyRes("images/icon-buttons/moon/light/focus.svg"),
      dottyRes("images/icon-buttons/moon/light/hover.svg"),
      dottyRes("images/icon-buttons/moon/light/selected.svg"),
      dottyRes("images/icon-buttons/moon/light/default.svg"),
      dottyRes("images/icon-buttons/search/dark/active.svg"),
      dottyRes("images/icon-buttons/search/dark/disabled.svg"),
      dottyRes("images/icon-buttons/search/dark/focus.svg"),
      dottyRes("images/icon-buttons/search/dark/hover.svg"),
      dottyRes("images/icon-buttons/search/dark/selected.svg"),
      dottyRes("images/icon-buttons/search/dark/default.svg"),
      dottyRes("images/icon-buttons/search/light/active.svg"),
      dottyRes("images/icon-buttons/search/light/disabled.svg"),
      dottyRes("images/icon-buttons/search/light/focus.svg"),
      dottyRes("images/icon-buttons/search/light/hover.svg"),
      dottyRes("images/icon-buttons/search/light/selected.svg"),
      dottyRes("images/icon-buttons/search/light/default.svg"),
      dottyRes("images/icon-buttons/arrow-down/dark/active.svg"),
      dottyRes("images/icon-buttons/arrow-down/dark/disabled.svg"),
      dottyRes("images/icon-buttons/arrow-down/dark/focus.svg"),
      dottyRes("images/icon-buttons/arrow-down/dark/hover.svg"),
      dottyRes("images/icon-buttons/arrow-down/dark/selected.svg"),
      dottyRes("images/icon-buttons/arrow-down/dark/default.svg"),
      dottyRes("images/icon-buttons/arrow-down/light/active.svg"),
      dottyRes("images/icon-buttons/arrow-down/light/disabled.svg"),
      dottyRes("images/icon-buttons/arrow-down/light/focus.svg"),
      dottyRes("images/icon-buttons/arrow-down/light/hover.svg"),
      dottyRes("images/icon-buttons/arrow-down/light/selected.svg"),
      dottyRes("images/icon-buttons/arrow-down/light/default.svg"),
      dottyRes("images/icon-buttons/arrow-right/dark/active.svg"),
      dottyRes("images/icon-buttons/arrow-right/dark/disabled.svg"),
      dottyRes("images/icon-buttons/arrow-right/dark/focus.svg"),
      dottyRes("images/icon-buttons/arrow-right/dark/hover.svg"),
      dottyRes("images/icon-buttons/arrow-right/dark/selected.svg"),
      dottyRes("images/icon-buttons/arrow-right/dark/default.svg"),
      dottyRes("images/icon-buttons/arrow-right/light/active.svg"),
      dottyRes("images/icon-buttons/arrow-right/light/disabled.svg"),
      dottyRes("images/icon-buttons/arrow-right/light/focus.svg"),
      dottyRes("images/icon-buttons/arrow-right/light/hover.svg"),
      dottyRes("images/icon-buttons/arrow-right/light/selected.svg"),
      dottyRes("images/icon-buttons/arrow-right/light/default.svg"),
      dottyRes("images/icon-buttons/close/light/active.svg"),
      dottyRes("images/icon-buttons/close/light/default.svg"),
      dottyRes("images/icon-buttons/close/light/disabled.svg"),
      dottyRes("images/icon-buttons/close/light/focus.svg"),
      dottyRes("images/icon-buttons/close/light/hover.svg"),
      dottyRes("images/icon-buttons/close/light/selected.svg"),
      dottyRes("images/icon-buttons/close/dark/active.svg"),
      dottyRes("images/icon-buttons/close/dark/default.svg"),
      dottyRes("images/icon-buttons/close/dark/disabled.svg"),
      dottyRes("images/icon-buttons/close/dark/focus.svg"),
      dottyRes("images/icon-buttons/close/dark/hover.svg"),
      dottyRes("images/icon-buttons/close/dark/selected.svg"),
      dottyRes("images/bulb/dark/default.svg"),
      dottyRes("images/bulb/light/default.svg"),
      dottyRes("images/info/light/default.svg"),
      dottyRes("images/info/dark/default.svg"),
      dottyRes("images/icon-buttons/menu-animated/dark/active.svg"),
      dottyRes("images/icon-buttons/menu-animated/dark/disabled.svg"),
      dottyRes("images/icon-buttons/menu-animated/dark/focus.svg"),
      dottyRes("images/icon-buttons/menu-animated/dark/hover.svg"),
      dottyRes("images/icon-buttons/menu-animated/dark/selected.svg"),
      dottyRes("images/icon-buttons/menu-animated/dark/default.svg"),
      dottyRes("images/icon-buttons/menu-animated/light/active.svg"),
      dottyRes("images/icon-buttons/menu-animated/light/disabled.svg"),
      dottyRes("images/icon-buttons/menu-animated/light/focus.svg"),
      dottyRes("images/icon-buttons/menu-animated/light/hover.svg"),
      dottyRes("images/icon-buttons/menu-animated/light/selected.svg"),
      dottyRes("images/icon-buttons/menu-animated/light/default.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/dark/active.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/dark/disabled.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/dark/focus.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/dark/hover.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/dark/selected.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/dark/default.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/light/active.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/light/disabled.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/light/focus.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/light/hover.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/light/selected.svg"),
      dottyRes("images/icon-buttons/menu-animated-open/light/default.svg"),
      dottyRes("images/footer-icon/dark/default.svg"),
      dottyRes("images/footer-icon/light/default.svg"),
      dottyRes("images/icon-buttons/gh/dark/active.svg"),
      dottyRes("images/icon-buttons/gh/dark/disabled.svg"),
      dottyRes("images/icon-buttons/gh/dark/focus.svg"),
      dottyRes("images/icon-buttons/gh/dark/hover.svg"),
      dottyRes("images/icon-buttons/gh/dark/selected.svg"),
      dottyRes("images/icon-buttons/gh/dark/default.svg"),
      dottyRes("images/icon-buttons/gh/light/active.svg"),
      dottyRes("images/icon-buttons/gh/light/disabled.svg"),
      dottyRes("images/icon-buttons/gh/light/focus.svg"),
      dottyRes("images/icon-buttons/gh/light/hover.svg"),
      dottyRes("images/icon-buttons/gh/light/selected.svg"),
      dottyRes("images/icon-buttons/gh/light/default.svg"),
      dottyRes("images/icon-buttons/twitter/dark/active.svg"),
      dottyRes("images/icon-buttons/twitter/dark/disabled.svg"),
      dottyRes("images/icon-buttons/twitter/dark/focus.svg"),
      dottyRes("images/icon-buttons/twitter/dark/hover.svg"),
      dottyRes("images/icon-buttons/twitter/dark/selected.svg"),
      dottyRes("images/icon-buttons/twitter/dark/default.svg"),
      dottyRes("images/icon-buttons/twitter/light/active.svg"),
      dottyRes("images/icon-buttons/twitter/light/disabled.svg"),
      dottyRes("images/icon-buttons/twitter/light/focus.svg"),
      dottyRes("images/icon-buttons/twitter/light/hover.svg"),
      dottyRes("images/icon-buttons/twitter/light/selected.svg"),
      dottyRes("images/icon-buttons/twitter/light/default.svg"),
      dottyRes("images/icon-buttons/discord/dark/active.svg"),
      dottyRes("images/icon-buttons/discord/dark/disabled.svg"),
      dottyRes("images/icon-buttons/discord/dark/focus.svg"),
      dottyRes("images/icon-buttons/discord/dark/hover.svg"),
      dottyRes("images/icon-buttons/discord/dark/selected.svg"),
      dottyRes("images/icon-buttons/discord/dark/default.svg"),
      dottyRes("images/icon-buttons/discord/light/active.svg"),
      dottyRes("images/icon-buttons/discord/light/disabled.svg"),
      dottyRes("images/icon-buttons/discord/light/focus.svg"),
      dottyRes("images/icon-buttons/discord/light/hover.svg"),
      dottyRes("images/icon-buttons/discord/light/selected.svg"),
      dottyRes("images/icon-buttons/discord/light/default.svg"),
      dottyRes("images/icon-buttons/gitter/dark/active.svg"),
      dottyRes("images/icon-buttons/gitter/dark/disabled.svg"),
      dottyRes("images/icon-buttons/gitter/dark/focus.svg"),
      dottyRes("images/icon-buttons/gitter/dark/hover.svg"),
      dottyRes("images/icon-buttons/gitter/dark/selected.svg"),
      dottyRes("images/icon-buttons/gitter/dark/default.svg"),
      dottyRes("images/icon-buttons/gitter/light/active.svg"),
      dottyRes("images/icon-buttons/gitter/light/disabled.svg"),
      dottyRes("images/icon-buttons/gitter/light/focus.svg"),
      dottyRes("images/icon-buttons/gitter/light/hover.svg"),
      dottyRes("images/icon-buttons/gitter/light/selected.svg"),
      dottyRes("images/icon-buttons/gitter/light/default.svg"),
      dottyRes("images/icon-buttons/link/dark/active.svg"),
      dottyRes("images/icon-buttons/link/dark/disabled.svg"),
      dottyRes("images/icon-buttons/link/dark/focus.svg"),
      dottyRes("images/icon-buttons/link/dark/hover.svg"),
      dottyRes("images/icon-buttons/link/dark/selected.svg"),
      dottyRes("images/icon-buttons/link/dark/default.svg"),
      dottyRes("images/icon-buttons/link/light/active.svg"),
      dottyRes("images/icon-buttons/link/light/disabled.svg"),
      dottyRes("images/icon-buttons/link/light/focus.svg"),
      dottyRes("images/icon-buttons/link/light/hover.svg"),
      dottyRes("images/icon-buttons/link/light/selected.svg"),
      dottyRes("images/icon-buttons/link/light/default.svg"),
      searchData(pages),
      scastieConfiguration(),
    )

  def renderResource(resource: Resource): Seq[String] =
    val normalizedPath = resource.path.replace('\\', '/')
    if normalizedPath.endsWith(".html") && apiPaths.contains(normalizedPath) then
      report.error(s"Conflict between resource and API member for $normalizedPath. $pathsConflictResoultionMsg")
      Nil
    else
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
        case Resource.URLToCopy(url, dest) =>
          Seq(copy(new URL(url).openStream(), dest))
