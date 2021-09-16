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

case class Page(link: Link, content: Member | ResolvedTemplate | String, children: Seq[Page]):
  def withNewChildren(newChildren: Seq[Page]) = copy(children = children ++ newChildren)

  def withTitle(newTitle: String) = copy(link = link.copy(name = newTitle))

  def hasFrame = content match
    case t: ResolvedTemplate => t.hasFrame
    case _ => true

class HtmlRenderer(rootPackage: Member, val members: Map[DRI, Member])(using ctx: DocContext)
  extends SiteRenderer, Resources, Locations, Writer:
  private val args = summon[DocContext].args
  val staticSite = summon[DocContext].staticSiteContext

  val effectiveMembers = members

  private def memberPage(member: Member): Page =
    val childrenPages = member.members.filter(_.needsOwnPage)
    Page(Link(member.name, member.dri), member, childrenPages.map(memberPage))

  val navigablePage: Page =
    val rootPckPage = memberPage(rootPackage)
    staticSite match
      case None => rootPckPage.withTitle(args.name)
      case Some(siteContext) =>
        val (indexes, templates) = siteContext.templates.partition(f =>
          f.templateFile.isIndexPage() && f.file.toPath.getParent() == siteContext.docsPath)
        if (indexes.size > 1)
          val msg = s"ERROR: Multiple index pages for doc found ${indexes.map(_.file)}"
          report.error(msg)

        val templatePages = templates.map(templateToPage(_, siteContext))

        indexes.headOption match
          case None if templatePages.isEmpty=>
            rootPckPage.withTitle(args.name)
          case None =>
            Page(Link(args.name, docsRootDRI),"", templatePages :+ rootPckPage.withTitle("API"))
          case Some(indexPage) =>
            val newChildren = templatePages :+ rootPckPage.withTitle("API")
            templateToPage(indexPage, siteContext).withNewChildren(newChildren)

  val hiddenPages: Seq[Page] =
    staticSite match
      case None =>
        Seq(navigablePage.copy( // Add index page that is a copy of api/index.html
          link = navigablePage.link.copy(dri = docsRootDRI),
          children = Nil
        ))
      case Some(siteContext) =>
        // In case that we do not have an index page and we do not have any API entries
        // we want to create empty index page, so there is one
        val actualIndexTemplate = siteContext.indexTemplate() match {
            case None if effectiveMembers.isEmpty => Seq(siteContext.emptyIndexTemplate)
            case templates => templates.toSeq
          }

          (siteContext.orphanedTemplates ++ actualIndexTemplate).map(templateToPage(_, siteContext))

  /**
   * Here we have to retrive index pages from hidden pages and replace fake index pages in navigable page tree.
   */
  val allPages: Seq[Page] =
    def traversePages(page: Page): (Page, Seq[Page]) =
      val (newChildren, newPagesToRemove): (Seq[Page], Seq[Page]) = page.children.map(traversePages(_)).foldLeft((Seq[Page](), Seq[Page]())) {
        case ((pAcc, ptrAcc), (p, ptr)) => (pAcc :+ p, ptrAcc ++ ptr)
      }
      hiddenPages.find(_.link == page.link) match
        case None =>
          (page.copy(children = newChildren), newPagesToRemove)
        case Some(newPage) =>
          (newPage.copy(children = newChildren), newPagesToRemove :+ newPage)

    val (newNavigablePage, pagesToRemove) = traversePages(navigablePage)

    val all = newNavigablePage +: hiddenPages.filterNot(pagesToRemove.contains)
    // We need to check for conflicts only if we have top-level member called blog or docs
    val hasPotentialConflict =
      rootPackage.members.exists(m => m.name.startsWith("docs") || m.name.startsWith("blog"))

    if hasPotentialConflict then
      def walk(page: Page): Unit =
        if page.link.dri.isStaticFile then
          val dest = absolutePath(page.link.dri)
          if apiPaths.contains(dest) then
            report.error(s"Conflict between static page and API member for $dest. $pathsConflictResoultionMsg")
          page.children.foreach(walk)

      all.foreach (walk)

    all

  def renderContent(page: Page) = page.content match
    case m: Member =>
      val signatureRenderer = new SignatureRenderer:
        def currentDri: DRI = page.link.dri
        def link(dri: DRI): Option[String] =
          Some(pathToPage(currentDri, dri)).filter(_ != UnresolvedLocationLink)

      MemberRenderer(signatureRenderer).fullMember(m)
    case t: ResolvedTemplate => siteContent(page.link.dri, t)
    case a: String =>  raw(a)


  def renderPage(page: Page, parents: Vector[Link]): Seq[String] =
    val newParents = parents :+ page.link
    val content = html(
      mkHead(page),
      body(
        if !page.hasFrame then renderContent(page)
        else mkFrame(page.link, newParents, renderContent(page))
      )
    )
    write(page.link.dri, content) +: page.children.flatMap(renderPage(_, newParents))

  private def specificResources(page: Page): Set[String] =
    page.children.toSet.flatMap(specificResources) ++ (page.content match
      case r: ResolvedTemplate =>
        r.resolved.resources.toSet
      case _ => Set.empty
    )

  private def renderResources(): Seq[String] =
    def siteRoot = staticSite.get.root.toPath
    def pathToResource(p: String) = Resource.File(p, siteRoot.resolve(p))

    def harvestResources(path: String) =
      val siteImgPath = siteRoot.resolve(path)
      if !Files.exists(siteImgPath) then Nil
      else
        val allPaths = Files.walk(siteImgPath, FileVisitOption.FOLLOW_LINKS)
        val files = allPaths.filter(Files.isRegularFile(_)).iterator().asScala
        files.map(p => siteRoot.relativize(p).toString).toList

    val staticResources = staticSite.toSeq.flatMap { _ =>
      harvestResources("images") ++ harvestResources("resources")
    }

    val siteResourcesPaths = allPages.toSet.flatMap(specificResources) ++ staticResources

    val resources = siteResourcesPaths.toSeq.map(pathToResource) ++ allResources(allPages) ++ onlyRenderedResources
    resources.flatMap(renderResource)

  def render(): Unit =
    val renderedResources = renderResources()
    val sites = allPages.map(renderPage(_, Vector.empty))

  def mkHead(page: Page): AppliedTag =
    val resources = page.content match
      case t: ResolvedTemplate =>
        t.resolved.resources ++ (if t.hasFrame then memberResourcesPaths else Nil)
      case _ =>
        memberResourcesPaths

    val earlyResources = page.content match
      case t: ResolvedTemplate => if t.hasFrame then earlyMemberResourcePaths else Nil
      case _ => earlyMemberResourcePaths

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

      nav.children match
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

  private def canonicalUrl(l: String): AppliedTag | String =
    val canon = args.docCanonicalBaseUrl
    if !canon.isEmpty then
      val canonicalUrl = if canon.endsWith("/") then canon else canon + "/"
      link(rel := "canonical", href := canonicalUrl + l)
    else
      "" // return empty tag

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
      div(cls := "breadcrumbs")(innerTags:_*)

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
                args.projectVersion.map(v => div(cls:="projectVersion")(v)).getOrElse("")
              ),
              div(id := "dropdown-content", cls := "dropdown-content")(
                input(`type` := "text", placeholder := "Search...", id := "dropdown-input", onkeyup := "filterFunction()"),
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
