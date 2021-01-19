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
import java.nio.file.FileVisitOption
import java.io.File
import org.jetbrains.dokka.pages.ContentNode

case class Page(link: Link, content: Member | ResolvedTemplate | String, children: Seq[Page]):
  def withNewChildren(newChildren: Seq[Page]) = copy(children = children ++ newChildren)

  def withTitle(newTitle: String) = copy(link = link.copy(name = newTitle))

  def hasFrame = content match
    case t: ResolvedTemplate => t.hasFrame
    case _ => true

class HtmlRenderer(rootPackage: Member, val members: Map[DRI, Member], buildNode: DRI => ContentNode => String)(using ctx: DocContext)
  extends SiteRenderer, Resources, Locations, Writter:
  private val args = summon[DocContext].args
  val staticSite = summon[DocContext].staticSiteContext

  private def memberPage(member: Member): Page =
    val childrenPages = member
      .membersBy(m => m.kind == Kind.Package || m.kind.isInstanceOf[Classlike])
      .filter(m => m.origin == Origin.RegularlyDefined && m.inheritedFrom.isEmpty)
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

        val templatePages =
          (templates ++ siteContext.indexTemplate()).map(templateToPage(_, siteContext))
        indexes.headOption match
          case None if templatePages.isEmpty=>
            rootPckPage.withTitle(args.name)
          case None =>
            Page(Link(args.name, docsRootDRI),"", templatePages :+ rootPckPage.withTitle("API"))
          case Some(indexPage) =>
            val newChildren = templatePages :+ rootPckPage.withTitle("API")
            templateToPage(indexPage, siteContext).withNewChildren(newChildren)

  val hiddenPages: Seq[Page] =
    staticSite.toSeq.flatMap(c => c.orphanedTemplates.map(templateToPage(_, c)))

  def renderContent(page: Page) = page.content match
    case m: Member =>
      val signatureRenderer = new SignatureRenderer:
        def currentDri: DRI = page.link.dri
        def link(dri: DRI): Option[String] = Some(pathToPage(page.link.dri, dri))

      MemberRenderer(signatureRenderer, buildNode(page.link.dri)).fullMember(m)
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

    val siteImages = staticSite.toSeq.flatMap { _ =>
      val siteImgPath = siteRoot.resolve("images")
      if !Files.exists(siteImgPath) then Nil
      else
        val allPaths = Files.walk(siteImgPath, FileVisitOption.FOLLOW_LINKS)
        val files = allPaths.filter(Files.isRegularFile(_)).iterator().asScala
        files.map(p => siteRoot.relativize(p).toString).toList
    }

    val siteResourcesPaths =
      (navigablePage +: hiddenPages).toSet.flatMap(specificResources) ++ siteImages

    val resources = siteResourcesPaths.toSeq.map(pathToResource) ++ allResources(rootPackage)
    resources.flatMap(renderResource)

  def render(): Unit =
    val renderedResources = renderResources()
    val sites = (navigablePage +: hiddenPages).map(renderPage(_, Vector.empty))

  def mkHead(page: Page): AppliedTag =
    val resources = page.content match
      case t: ResolvedTemplate =>
        t.resolved.resources ++ (if t.hasFrame then memberResourcesPaths else Nil)
      case _ =>
        memberResourcesPaths

    head(
      meta(charset := "utf-8"),
      meta(HTML.name := "viewport", content := "width=device-width, initial-scale=1"),
      title(page.link.name),
      link(
        rel := "shortcut icon",
        `type` := "image/x-icon",
        href := resolveLink(page.link.dri, "favicon.ico")
      ),
      linkResources(page.link.dri, resources).toList,
      script(raw(s"""var pathToRoot = "${pathToRoot(page.link.dri)}";"""))
    )

  private def buildNavigation(pageLink: Link): AppliedTag =
    def renderNested(nav: Page): (Boolean, AppliedTag) =
      val isSelected = nav.link.dri == pageLink.dri
      def linkHtml(exapnded: Boolean = false) =
        val attrs = if (isSelected) Seq(cls := "selected expanded") else Nil
        a(href := pathToPage(nav.link.dri, pageLink.dri), attrs)(nav.link.name)

      nav.children match
        case Nil => isSelected -> div(linkHtml())
        case children =>
          val nested = children.map(renderNested)
          val expanded = nested.exists(_._1) || nav.link == pageLink
          val attr = if expanded || isSelected then Seq(cls := "expanded") else Nil
          (isSelected || expanded) -> div(attr)(
            linkHtml(expanded),
            span(),
            nested.map(_._2)
          )
    renderNested(navigablePage)._2

  private def mkFrame(link: Link, parents: Vector[Link], content: => AppliedTag): AppliedTag =
    val projectLogo =
      args.projectLogo.map { path =>
        val fileName = Paths.get(path).getFileName()
        span(img(src := resolveRoot(link.dri, s"project-logo/$fileName")))
      }.toSeq

    val parentsHtml =
      val innerTags = parents.flatMap[TagArg](b => Seq(
          a(href := pathToPage(b.dri, link.dri))(b.name),
          "/"
        )).dropRight(1)
      div(cls := "breadcrumbs")(innerTags:_*)

    div(id := "container")(
      div(id := "leftColumn")(
        div(id := "logo")(
          projectLogo,
          span(
            div(cls:="projectName")(args.name)
          ),
          span(
            args.projectVersion.map(v => div(cls:="projectVersion")(v)).toList
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
        div(id := "searchBar"),
        main(
          div(id := "content")(
            parentsHtml,
            div(content),
          )
        ),
        footer(
          span(cls := "go-to-top-icon")(
            a(href := "#container")(
              span(cls:="icon-vertical_align_top"),
              raw("&nbsp;Back to top")
            )
          ),
          raw("Generated by&nbsp;"),
          a(href := "https://github.com/lampepfl/dotty/tree/master/scala3doc")(
            img(
              src := resolveRoot(link.dri, "images/scala3doc_logo.svg"),
              alt := "Scala3doc",
              cls := "scala3doc_logo"
            )
          )
        )
      )
    )
