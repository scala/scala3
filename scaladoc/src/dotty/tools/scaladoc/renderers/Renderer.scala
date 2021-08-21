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

abstract class Renderer(rootPackage: Member, val members: Map[DRI, Member], protected val extension: String = "html")(using ctx: DocContext)
  extends SiteRenderer, Resources, Locations, Writer:
  protected val args = summon[DocContext].args
  val staticSite = summon[DocContext].staticSiteContext

  val effectiveMembers = members

  protected def memberPage(member: Member): Page =
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

      all.foreach(walk)

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



  protected def canonicalUrl(l: String): AppliedTag | String =
    val canon = args.docCanonicalBaseUrl
    if !canon.isEmpty then
      val canonicalUrl = if canon.endsWith("/") then canon else canon + "/"
      link(rel := "canonical", href := canonicalUrl + l)
    else
      "" // return empty tag

  /**
   * Main method rendering all the pages
   */
  def render(): Unit =
    val sites = allPages.map(renderPage(_, Vector.empty))

  /**
   * Handler to prepare the content to be rendered. It's a good place to organize frame, footers, front-matter, etc.
   */
  def pageContent(page: Page, parents: Vector[Link]): AppliedTag

  /**
   * Method to be overriden by concrete renderer to render single page
   */
  def renderPage(page: Page, parents: Vector[Link]): Seq[String] =
    val newParents = parents :+ page.link
    val content = pageContent(page, newParents)
    write(page.link.dri, content, extension) +: page.children.flatMap(renderPage(_, newParents))


