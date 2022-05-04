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
        if !page.hasFrame then renderContent(page).content
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

  private def buildNavigation(pageLink: Link): (Option[(Boolean, Seq[AppliedTag])], Option[(Boolean, Seq[AppliedTag])]) =
    def navigationIcon(member: Member) = member match {
      case m if m.needsOwnPage => Seq(span(cls := s"micon ${member.kind.name.take(2)}"))
      case _ => Nil
    }

    def renderNested(nav: Page, nestLevel: Int): (Boolean, AppliedTag) =
      val isApi = nav.content.isInstanceOf[Member]
      val isSelected = nav.link.dri == pageLink.dri
      val isTopElement = nestLevel == 0

      def linkHtml(expanded: Boolean = false, withArrow: Boolean = false) =
        val attrs: Seq[String] = Seq(
          Option.when(isSelected || expanded)("h100"),
          Option.when(isSelected)("selected"),
          Option.when(expanded)("expanded cs"),
          Option.when(!isApi)("de"),
        ).flatten
        val icon = nav.content match {
          case m: Member => navigationIcon(m)
          case _ => Nil
        }
        Seq(
          span(cls := s"nh " + attrs.mkString(" "))(
            if withArrow then Seq(button(cls := s"ar icon-button ${if isSelected || expanded then "expanded" else ""}")) else Nil,
            a(href := pathToPage(pageLink.dri, nav.link.dri))(icon, span(nav.link.name))
          )
        )

      nav.children.filterNot(_.hidden) match
        case Nil => isSelected -> div(cls := s"ni n$nestLevel ${if isSelected then "expanded" else ""}")(linkHtml())
        case children =>
          val nested = children.map(renderNested(_, nestLevel + 1))
          val expanded = nested.exists(_._1)
          val attr =
            if expanded || isSelected then Seq(cls := s"ni n$nestLevel expanded") else Seq(cls := s"ni n$nestLevel")
          (isSelected || expanded) -> div(attr)(
            linkHtml(expanded, true),
            nested.map(_._2)
          )

    val isRootApiPageSelected = rootApiPage.fold(false)(_.link.dri == pageLink.dri)
    val isDocsApiPageSelected = rootDocsPage.fold(false)(_.link.dri == pageLink.dri)
    val apiNav = rootApiPage.map { p => p.children.map(renderNested(_, 0)) match
      case entries => (entries.exists(_._1) || isRootApiPageSelected, entries.map(_._2))
    }
    val docsNav = rootDocsPage.map { p => p.children.map(renderNested(_, 0)) match
      case entries => (entries.exists(_._1) || isDocsApiPageSelected, entries.map(_._2))
    }

    (apiNav, docsNav)

  private def hasSocialLinks = !args.socialLinks.isEmpty

  private def socialLinks(whiteIcon: Boolean = true) =
    val icon = (link: SocialLinks) => if whiteIcon then link.whiteIconName else link.blackIconName
    args.socialLinks.map { link =>
      a(href := link.url)(
        span(cls := s"social-icon", Attr("data-icon-path") := icon(link))
      )
    }

  private def renderTableOfContents(toc: Seq[TocEntry]): Option[AppliedTag] =
    def renderTocRec(level: Int, rest: Seq[TocEntry]): Seq[AppliedTag] =
      rest match {
        case Nil => Nil
        case head :: tail if head.level == level =>
          val (nested, rest) = tail.span(_.level > level)
          val nestedList = if nested.nonEmpty then Seq(ul(renderTocRec(level + 1, nested))) else Nil
          li(a(href := head.anchor)(head.content), nestedList) +: renderTocRec(level, rest)
        case rest @ (head :: tail) if head.level > level =>
          val (prefix, suffix) = rest.span(_.level > level)
          li(ul(renderTocRec(level + 1, prefix))) +: renderTocRec(level, suffix)
      }

    renderTocRec(1, toc).headOption.map(toc => nav(cls := "toc-nav")(ul(cls := "toc-list")(toc)))


  private def mkFrame(link: Link, parents: Vector[Link], content: => PageContent): AppliedTag =
    val projectLogoElem =
      projectLogo.flatMap {
        case Resource.File(path, _) =>
          Some(span(id := "project-logo", cls := "project-logo")(img(src := resolveRoot(link.dri, path))))
        case _ => None
      }

    val darkProjectLogoElem =
      darkProjectLogo.flatMap {
        case Resource.File(path, _) =>
          Some(span(id := "dark-project-logo", cls := "project-logo")(img(src := resolveRoot(link.dri, path))))
        case _ => None
      }.orElse(projectLogoElem)

    val parentsHtml =
      val innerTags = parents.flatMap[TagArg](b => Seq(
          a(href := pathToPage(link.dri, b.dri))(b.name),
          "/"
        )).dropRight(1)
      div(cls := "breadcrumbs container")(innerTags:_*)

    val (apiNavOpt, docsNavOpt): (Option[(Boolean, Seq[AppliedTag])], Option[(Boolean, Seq[AppliedTag])]) = buildNavigation(link)

    def textFooter: String | AppliedTag =
      args.projectFooter.fold("") { f =>
        span(id := "footer-text")(
          raw(f)
        )
      }

    div(id := "container")(
      div(id := "header")(
        div(cls := "header-container-left")(
          projectLogoElem.toSeq,
          darkProjectLogoElem.toSeq,
          span(onclick := "dropdownHandler(event)", cls := "text-button with-arrow", id := "dropdown-trigger")(
            a()(
              args.projectVersion.map(v => div(cls:="projectVersion")(v)).getOrElse("")
            ),
          ),
          div(id := "version-dropdown", cls := "dropdown-menu") ()
        ),
         div(cls:="header-container-right")(
          button(id := "search-toggle", cls := "icon-button"),
            a(href := "https://www.scala-lang.org/download/", cls := "text-button") (
              "Download",
            ),
            a(href := "https://docs.scala-lang.org/", cls := "text-button") (
              "Documentation",
            ),
            a(href := "https://index.scala-lang.org", cls := "text-button") (
              "Libraries",
            ),
            a(href := "https://www.scala-lang.org/contribute/", cls := "text-button") (
              "Contribute",
            ),
            a(href := "https://www.scala-lang.org/contribute/", cls := "text-button") (
              "Blog",
            ),
            a(href := "https://www.scala-lang.org/blog/", cls := "text-button") (
              "Community",
            ),
          button(id := "theme-toggle", cls := "icon-button"),
          button(id := "mobile-menu-toggle", cls := "icon-button hamburger"),
        ),
      ),
      div(id := "mobile-menu")(
        div(cls := "mobile-menu-header")(
          span(cls := "mobile-menu-logo")(
            projectLogoElem.toSeq,
            darkProjectLogoElem.toSeq,
          ),
          button(id := "mobile-menu-close", cls := "icon-button close"),
        ),
        div(cls := "mobile-menu-container body-medium")(
          a(href := "https://www.scala-lang.org/download/", cls := "mobile-menu-item") (
            "Download",
          ),
          a(href := "https://docs.scala-lang.org/", cls := "mobile-menu-item") (
            "Documentation",
          ),
          a(href := "https://index.scala-lang.org", cls := "mobile-menu-item") (
            "Libraries",
          ),
          a(href := "https://www.scala-lang.org/contribute/", cls := "mobile-menu-item contribute") (
            "Contribute",
          ),
          a(href := "https://www.scala-lang.org/contribute/", cls := "mobile-menu-item") (
            "Blog",
          ),
          a(href := "https://www.scala-lang.org/blog/", cls := "mobile-menu-item") (
            "Community",
          ),
          span(id := "mobile-theme-toggle", cls := "mobile-menu-item mode"),
        )
      ),
      button(id := "mobile-sidebar-toggle", cls := "floating-button"),
      div(id := "leftColumn", cls := "body-small")(
        Seq(
          div(cls:= "switcher-container")(
            apiNavOpt match {
              case Some(isApiActive, apiNav) =>
                Seq(a(id := "api-nav-button", cls:= s"switcher h100 ${if isApiActive then "selected" else ""}", href := pathToPage(link.dri, rootApiPage.get.link.dri))("API"))
              case _ => Nil
            },
            docsNavOpt match {
              case Some(isDocsActive, docsNav) =>
                Seq(a(id := "docs-nav-button", cls:= s"switcher h100 ${if isDocsActive then "selected" else ""}", href := pathToPage(link.dri, rootDocsPage.get.link.dri))("Docs"))
              case _ => Nil
            }
          ),
          apiNavOpt
            .filter(_._1)
            .map(apiNav => nav(id := "api-nav", cls := s"side-menu")(apiNav._2))
            .orElse(docsNavOpt.map(docsNav => nav(id := "docs-nav", cls := s"side-menu")(docsNav._2)))
            .get
        )
      ),
      div(id := "footer", cls := "body-small")(
        div(cls := "left-container")(
         "Generated with"
        ),
        div(cls := "right-container")(
          a(href := "https://github.com/lampepfl/dotty") (
            button(cls := "icon-button gh")
          ),
          a(href := "https://twitter.com/scala_lang") (
            button(cls := "icon-button twitter")
          ),
          a(href := "https://discord.com/invite/scala") (
            button(cls := "icon-button discord"),
          ),
          a(href := "https://gitter.im/scala/scala") (
            button(cls := "icon-button gitter"),
          ),
          div(cls := "text")(
            "© 2002-2021 · LAMP/EPFL"
          )
        ),
        div(cls := "text-mobile")(
          "© 2002-2021 · LAMP/EPFL"
        )
      ),
      div(id := "main")(
        div (id := "leftToggler")(
          span(cls := "icon-toggler")
        ),
        div(id := "scaladoc-searchBar"),
      ),
      renderTableOfContents(content.toc).fold(Nil) { toc =>
        div(id := "toc")(
          span(cls := "toc-title")("In this article"),
          toc
        )
      }
    )