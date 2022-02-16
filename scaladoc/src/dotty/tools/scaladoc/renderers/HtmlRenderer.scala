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
import java.nio.file.FileVisitOption
import java.io.File
import dotty.tools.scaladoc.staticFileSymbolUUID

class HtmlRenderer(rootPackage: Member, members: Map[DRI, Member])(using ctx: DocContext)
  extends Renderer(rootPackage, members, extension = "html"):

  override def pageContent(page: Page, parents: Vector[Link]): AppliedTag =
    html(
      mkHead(page),
      body(
        if !page.hasFrame then renderContent(page)
        else mkFrame(page.link, parents, renderContent(page))
      )
    )

  override def render(): Unit =
    val renderedResources = renderResources()
    super.render()

  private def renderResources(): Seq[String] =
    import scala.util.Using
    import scala.jdk.CollectionConverters._
    // All static site resources need to be in _assets folder
    val staticSiteResources = staticSite
      .map(_.root.toPath.resolve("_assets").toFile)
      .filter(f => f.exists && f.isDirectory)
      .toSeq
      .flatMap { resourceFile =>
        resourceFile.listFiles.toSeq.map(_.toPath).flatMap { file =>
          Using(Files.walk(file)) { stream =>
            stream.iterator().asScala.toSeq
              .map(from => Resource.File(resourceFile.toPath.relativize(from).toString, from))
          }.fold (
            { t =>
              report.warn(s"Error occured while processing _assets file.", t)
              Seq.empty
            },
            identity
          )
        }
      }
    val resources = staticSiteResources ++ allResources(allPages) ++ onlyRenderedResources
    resources.flatMap(renderResource)

  def mkHead(page: Page): AppliedTag =
    val resources = page.content match
      case t: ResolvedTemplate =>
        t.resolved.resources ++ (if t.hasFrame then commonResourcesPaths ++ staticSiteOnlyResourcesPaths else Nil)
      case _ =>
        commonResourcesPaths ++ apiOnlyResourcesPaths

    val earlyResources = page.content match
      case t: ResolvedTemplate => if t.hasFrame then earlyCommonResourcePaths else Nil
      case _ => earlyCommonResourcePaths

    head(
      meta(charset := "utf-8"),
      meta(util.HTML.name := "viewport", content := "width=device-width, initial-scale=1"),
      title(page.link.name),
      canonicalUrl(absolutePath(page.link.dri)),
      link(
        rel := "shortcut icon",
        `type` := "image/x-icon",
        href := resolveLink(page.link.dri, "favicon.ico")
      ),
      linkResources(page.link.dri, earlyResources, deferJs = false).toList,
      linkResources(page.link.dri, resources, deferJs = true).toList,
      script(raw(s"""var pathToRoot = "${pathToRoot(page.link.dri)}";""")),
      (page.content match
        case ResolvedTemplate(loadedTemplate, _) =>
          val path = loadedTemplate.templateFile.file.toPath
          ctx.sourceLinks.repoSummary(path) match
            case Some(DefinedRepoSummary("github", org, repo)) =>
              val tag: TagArg = ctx.sourceLinks.fullPath(relativePath(path)).fold("") { githubContributors =>
                Seq(
                  script(raw(s"""var githubContributorsUrl = "https://api.github.com/repos/$org/$repo";""")),
                  script(raw(s"""var githubContributorsFilename = "$githubContributors";"""))
                )
              }
              tag // for some reason inference fails so had to state the type explicitly
            case _ => ""
        case _ => ""),
      ctx.args.versionsDictionaryUrl match
        case Some(url) => script(raw(s"""var versionsDictionaryUrl = "$url";"""))
        case None => ""
    )

  private def buildNavigation(pageLink: Link): AppliedTag =
    def navigationIcon(member: Member) = member match {
      case m if m.needsOwnPage => Seq(span(cls := s"micon ${member.kind.name.take(2)}"))
      case _ => Nil
    }

    def renderNested(nav: Page, toplevel: Boolean = false): (Boolean, AppliedTag) =
      val isSelected = nav.link.dri == pageLink.dri

      def linkHtml(expanded: Boolean = false, withArrow: Boolean = false) =
        val attrs: Seq[String] = Seq(
          Option.when(isSelected)("selected"),
          Option.when(expanded)("expanded")
        ).flatten
        val icon = nav.content match {
          case m: Member => navigationIcon(m)
          case _ => Nil
        }
        Seq(
          span(cls := "nh " + attrs.mkString(" "))(
            if withArrow then Seq(span(cls := "ar")) else Nil,
            a(href := pathToPage(pageLink.dri, nav.link.dri))(icon, span(nav.link.name))
          )
        )

      nav.children.filterNot(_.hidden) match
        case Nil => isSelected -> div(cls := s"ni ${if isSelected then "expanded" else ""}")(linkHtml())
        case children =>
          val nested = children.map(renderNested(_))
          val expanded = nested.exists(_._1) || isSelected
          val attr =
            if expanded || isSelected || toplevel then Seq(cls := "ni expanded") else Seq(cls := "ni")
          (isSelected || expanded) -> div(attr)(
            linkHtml(expanded, true),
            nested.map(_._2)
          )

    renderNested(navigablePage, toplevel = true)._2

  private def hasSocialLinks = !args.socialLinks.isEmpty

  private def socialLinks(whiteIcon: Boolean = true) =
    val icon = (link: SocialLinks) => if whiteIcon then link.whiteIconName else link.blackIconName
    args.socialLinks.map { link =>
      a(href := link.url)(
        span(cls := s"social-icon", Attr("data-icon-path") := icon(link))
      )
    }

  private def mkFrame(link: Link, parents: Vector[Link], content: => AppliedTag): AppliedTag =
    val projectLogo =
      args.projectLogo.map { path =>
        val fileName = Paths.get(path).getFileName()
        span(img(src := resolveRoot(link.dri, s"project-logo/$fileName")))
      }.toSeq

    val parentsHtml =
      val innerTags = parents.flatMap[TagArg](b => Seq(
          a(href := pathToPage(link.dri, b.dri))(b.name),
          "/"
        )).dropRight(1)
      div(cls := "breadcrumbs container")(innerTags:_*)

    def textFooter: String | AppliedTag =
      args.projectFooter.fold("") { f =>
        span(id := "footer-text")(
          raw(f)
        )
      }

    div(id := "container")(
      div(id := "leftColumn")(
        div(id := "logo")(
          projectLogo,
          span(
            div(cls:="projectName")(args.name)
          ),
          div(id := "version")(
            div(cls := "versions-dropdown")(
              div(onclick := "dropdownHandler()", id := "dropdown-button", cls := "dropdownbtn dropdownbtnactive")(
                args.projectVersion.map(v => div(cls:="projectVersion")(v)).getOrElse(""),
                div(id := "dropdown-content", cls := "dropdown-content")(
                  input(`type` := "text", placeholder := "Search...", id := "dropdown-input", onkeyup := "filterFunction()"),
                ),
              ),
            )
          ),
          div(cls := "socials")(
            socialLinks()
          )
        ),
        div(id := "paneSearch"),
        nav(id := "sideMenu2")(
          buildNavigation(link)
        ),
      ),
      div(id := "main")(
        div (id := "leftToggler")(
          span(cls := "icon-toggler")
        ),
        div(id := "scaladoc-searchBar"),
        main(id := "main-content")(
          parentsHtml,
          div(id := "content")(content),
        ),
        footer(
          div(id := "generated-by")(
            span(cls := "footer-text")(raw("Generated by")),
            a(href := "https://github.com/lampepfl/dotty/tree/master/scaladoc")(
              img(
                src := resolveRoot(link.dri, "images/scaladoc_logo.svg"),
                alt := "scaladoc",
                cls := "scaladoc_logo"
              ),
              img(
                src := resolveRoot(link.dri, "images/scaladoc_logo_dark.svg"),
                alt := "scaladoc",
                cls := "scaladoc_logo_dark"
              )
            )
          ),
          textFooter,
          div(cls := "socials")(
            span(cls := "footer-text")(if hasSocialLinks then Seq(raw("Social links")) else Nil),
            socialLinks(whiteIcon = false)
          ),
          div(cls := "mode")(
            span(cls :="footer-text")(raw("Mode")),
            label(id := "theme-toggle", cls := "switch")(
              input(`type` := "checkbox"),
              span(cls := "slider")
            )
          ),
          span(cls := "go-to-top-icon")(
            a(href := "#container")(
              span(cls:="icon-vertical_align_top"),
              span(cls :="footer-text")(raw("Back to top"))
            )
          )
        )
      )
    )
