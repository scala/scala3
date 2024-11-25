package dotty.tools.scaladoc
package renderers

import util.HTML._
import java.net.{URI, URL}
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
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
      FilterAttributes.defaultValues.toSeq.map { case  (n, v) => n -> jsonString(v) }*
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
      "styles/theme/bundle.css",
      "styles/theme/components/bundle.css",
      "styles/theme/components/button/bundle.css",
      "styles/theme/layout/bundle.css",
      "styles/nord-light.css",
      "styles/dotty-icons.css",
      "styles/filter-bar.css",
      "styles/code-snippets.css",
      "styles/searchbar.css",
      "styles/social-links.css",
      "styles/versions-dropdown.css",
      "styles/content-contributors.css",
      "styles/fontawesome.css",
      "styles/staticsitestyles.css",
      "scripts/staticsite/alt-details.js",
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
      "scripts/scaladoc-scalajs.js",
      "scripts/contributors.js",
      "scripts/staticsite/main.js"

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
    // "styles/staticsitestyles.css",

  ).map(dottyRes)

  val searchDataPath = "scripts/searchData.js"
  val scastieConfigurationPath = "scripts/scastieConfiguration.js"
  val commonResourcesPaths = Seq(searchDataPath) ++ Seq(scastieConfigurationPath) ++ commonResources.map(_.path)
  val earlyCommonResourcePaths = earlyCommonResources.map(_.path)

  val apiOnlyResourcesPaths: Seq[String] = apiOnlyResources.map(_.path)
  val staticSiteOnlyResourcesPaths: Seq[String] = staticSiteOnlyResources.map(_.path)

  def searchData(pages: Seq[Page]) =
    val signatureProvider = ScalaSignatureProvider()
    def flattenToText(signature: Signature): String = signature.getName

    def mkEntry(
      dri: DRI,
      name: String,
      text: String,
      extensionTarget: String,
      descr: String,
      extraDescr: String,
      kind: String,
    ) = jsonObject(
        "l" -> jsonString(relativeInternalOrAbsoluteExternalPath(dri)),
        "e" -> (if dri.externalLink.isDefined then rawJSON("true") else rawJSON("false")),
        "i" -> jsonString(extensionTarget),
        "n" -> jsonString(name),
        "t" -> jsonString(text),
        "d" -> jsonString(descr),
        "k" -> jsonString(kind),
        "x" -> jsonString(extraDescr),
      )

    def extensionTarget(member: Member): String =
      member.kind match
        case Kind.Extension(on, _) =>
          val typeSig = SignatureBuilder()
            .keyword("extension ")
            .typeParamList(on.typeParams)
            .content
          val argsSig = SignatureBuilder()
            .functionTermParameters(on.argsLists)
            .content
          flattenToText(typeSig ++ argsSig)
        case _ => ""

    def docPartRenderPlain(d: DocPart): String =
      import dotty.tools.scaladoc.tasty.comments.wiki._
      def renderPlain(wd: WikiDocElement): String =
        wd match
          case Paragraph(text) => renderPlain(text)
          case Chain(items) => items.map(renderPlain).mkString("")
          case Italic(text) => renderPlain(text)
          case Bold(text) => renderPlain(text)
          case Underline(text) => renderPlain(text)
          case Superscript(text) => renderPlain(text)
          case Subscript(text) => renderPlain(text)
          case Link(link, title) => title.map(renderPlain).getOrElse(
            link match
              case DocLink.ToURL(url) => url
              case DocLink.ToDRI(_, name) => name
              case _ => ""
          )
          case Monospace(text) => renderPlain(text)
          case Text(text) => text
          case Summary(text) => renderPlain(text)
          case _ => ""
      d match
        case s: Seq[WikiDocElement @unchecked] =>
          if s.length == 0 then ""
          else renderPlain(s.head)
        case _ => ""

    def processPage(page: Page, pageFQName: List[String]): Seq[(JSON, Seq[String])] =
      val (res, pageName) =  page.content match
        case m: Member if m.kind != Kind.RootPackage =>
          def processMember(member: Member, fqName: List[String]): Seq[(JSON, Seq[String])] =
            val signature: MemberSignature = signatureProvider.rawSignature(member)()
            val sig = Signature(Plain(member.name)) ++ signature.suffix
            val descr = if member.kind == Kind.Package then "" else fqName.mkString(".")
            val extraDescr = member.docs.map(d => docPartRenderPlain(d.body)).getOrElse("")
            val entry = mkEntry(
              member.dri,
              member.name,
              flattenToText(sig),
              extensionTarget(member),
              descr,
              extraDescr,
              member.kind.name,
            )
            val children = member
                .membersBy(m => m.kind != Kind.Package && !m.kind.isInstanceOf[Classlike])
                .filter(m => m.origin == Origin.RegularlyDefined && m.inheritedFrom.fold(true)(_.isSourceSuperclassHidden))
            val updatedFqName = if member.kind == Kind.Package then List(member.name) else fqName :+ member.name
            Seq((entry, updatedFqName)) ++ children.flatMap(processMember(_, updatedFqName))

          (processMember(m, pageFQName), m.name)
        case _ =>
          (Seq((mkEntry(page.link.dri, page.link.name, page.link.name, "", "", "", "static"), pageFQName)), "")

      val updatedFqName = page.content match
        case m: Member if m.kind == Kind.Package => List(m.name)
        case _ if pageName.isEmpty => pageFQName
        case _ => pageFQName :+ pageName
      res ++ page.children.flatMap(processPage(_, updatedFqName))

    val entries = pages.flatMap(processPage(_, Nil))
    Resource.Text(searchDataPath, s"pages = ${jsonList(entries.map(_._1))};")

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
      dottyRes("images/no-results-icon.svg"),
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
      dottyRes("images/icon-buttons/copy/dark/active.svg"),
      dottyRes("images/icon-buttons/copy/dark/disabled.svg"),
      dottyRes("images/icon-buttons/copy/dark/focus.svg"),
      dottyRes("images/icon-buttons/copy/dark/hover.svg"),
      dottyRes("images/icon-buttons/copy/dark/selected.svg"),
      dottyRes("images/icon-buttons/copy/dark/default.svg"),
      dottyRes("images/icon-buttons/copy/light/active.svg"),
      dottyRes("images/icon-buttons/copy/light/disabled.svg"),
      dottyRes("images/icon-buttons/copy/light/focus.svg"),
      dottyRes("images/icon-buttons/copy/light/hover.svg"),
      dottyRes("images/icon-buttons/copy/light/selected.svg"),
      dottyRes("images/icon-buttons/copy/light/default.svg"),
      dottyRes("images/icon-buttons/plus/dark/active.svg"),
      dottyRes("images/icon-buttons/plus/dark/disabled.svg"),
      dottyRes("images/icon-buttons/plus/dark/focus.svg"),
      dottyRes("images/icon-buttons/plus/dark/hover.svg"),
      dottyRes("images/icon-buttons/plus/dark/selected.svg"),
      dottyRes("images/icon-buttons/plus/dark/default.svg"),
      dottyRes("images/icon-buttons/plus/light/active.svg"),
      dottyRes("images/icon-buttons/plus/light/disabled.svg"),
      dottyRes("images/icon-buttons/plus/light/focus.svg"),
      dottyRes("images/icon-buttons/plus/light/hover.svg"),
      dottyRes("images/icon-buttons/plus/light/selected.svg"),
      dottyRes("images/icon-buttons/plus/light/default.svg"),
      dottyRes("images/icon-buttons/minus/dark/active.svg"),
      dottyRes("images/icon-buttons/minus/dark/disabled.svg"),
      dottyRes("images/icon-buttons/minus/dark/focus.svg"),
      dottyRes("images/icon-buttons/minus/dark/hover.svg"),
      dottyRes("images/icon-buttons/minus/dark/selected.svg"),
      dottyRes("images/icon-buttons/minus/dark/default.svg"),
      dottyRes("images/icon-buttons/minus/light/active.svg"),
      dottyRes("images/icon-buttons/minus/light/disabled.svg"),
      dottyRes("images/icon-buttons/minus/light/focus.svg"),
      dottyRes("images/icon-buttons/minus/light/hover.svg"),
      dottyRes("images/icon-buttons/minus/light/selected.svg"),
      dottyRes("images/icon-buttons/minus/light/default.svg"),
      dottyRes("images/type-dark-big.svg"),
      dottyRes("images/type-big.svg"),
      dottyRes("images/enum-dark-big.svg"),
      dottyRes("images/enum-big.svg"),
      dottyRes("images/method-dark-big.svg"),
      dottyRes("images/method-big.svg"),
      dottyRes("images/given-dark-big.svg"),
      dottyRes("images/given-big.svg"),
      dottyRes("images/static-dark-big.svg"),
      dottyRes("images/static-big.svg"),
      dottyRes("images/def-dark-big.svg"),
      dottyRes("images/def-big.svg"),
      dottyRes("images/val-dark-big.svg"),
      dottyRes("images/val-big.svg"),
      dottyRes("images/trait-dark-big.svg"),
      dottyRes("images/trait-big.svg"),
      dottyRes("images/object-dark-big.svg"),
      dottyRes("images/object-big.svg"),
      dottyRes("images/class-dark-big.svg"),
      dottyRes("images/class-big.svg"),
      dottyRes("images/package-dark-big.svg"),
      dottyRes("images/package-big.svg"),
      dottyRes("images/thick.svg"),
      dottyRes("images/thick-dark.svg"),
      dottyRes("images/banner-icons/error.svg"),
      dottyRes("images/banner-icons/info.svg"),
      dottyRes("images/banner-icons/neutral.svg"),
      dottyRes("images/banner-icons/success.svg"),
      dottyRes("images/banner-icons/warning.svg"),
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
          Seq(copy(URI(url).toURL.openStream(), dest))
