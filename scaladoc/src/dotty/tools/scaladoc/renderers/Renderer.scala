package dotty.tools.scaladoc
package renderers

import util.HTML._
import collection.JavaConverters._
import collection.mutable.ListBuffer
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

        // Below code is for walking in order the tree and modifing its nodes basing on its neighbours

        // First we flatten templates to get them sorted in-order
        def flattenedTemplates(template: LoadedTemplate): Seq[LoadedTemplate] =
          template +: template.children.flatMap(flattenedTemplates)

        // We add dummy guards
        val allTemplates: Seq[Option[LoadedTemplate]] = None +: templates.flatMap(flattenedTemplates).map(Some(_)) :+ None

        // Let's gather the list of maps for each template with its in-order neighbours
        val newSettings: List[Map[String, Object]] = allTemplates.sliding(size = 3, step = 1).map {
          case None :: None :: Nil =>
            Map.empty
          case prev :: mid :: next :: Nil =>
            def link(sibling: Option[LoadedTemplate]): Option[String] =
              def realPath(path: Path) = if Files.isDirectory(path) then Paths.get(path.toString, "index.html") else path
              sibling.map { n =>
                val realMidPath = realPath(mid.get.file.toPath)
                val realSiblingPath = realPath(n.file.toPath)
                realMidPath.relativize(realSiblingPath).toString.stripPrefix("../")
              }
            List(link(prev).map("previous" -> _), link(next).map("next" -> _)).flatten.toMap
        }.toList

        def updateSettings(templates: Seq[LoadedTemplate], additionalSettings: ListBuffer[Map[String, Object]]): List[LoadedTemplate] =
          val updatedTemplates = List.newBuilder[LoadedTemplate]
          for template <- templates do
            val head: Map[String, Object] = additionalSettings.remove(0)
            val current: Map[String, Object] = template.templateFile.settings.getOrElse("page", Map.empty).asInstanceOf[Map[String, Object]]
            val updatedTemplateFile = template.templateFile.copy(settings = template.templateFile.settings.updated("page", head ++ current))
            updatedTemplates += template.copy(
              templateFile = updatedTemplateFile,
              children = updateSettings(template.children, additionalSettings)
            )
          updatedTemplates.result()

        val newTemplates = updateSettings(templates, newSettings.to(ListBuffer))
        val templatePages = newTemplates.map(templateToPage(_, siteContext))

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

  val redirectPages: Seq[Page] = staticSite.fold(Seq.empty)(siteContext => siteContext.redirectTemplates.map {
    case (template, driFrom, driTo) =>
      val redirectTo = pathToPage(driFrom, driTo)
      templateToPage(template.copy(templateFile = template.templateFile.copy(settings = template.templateFile.settings ++ Map("redirectTo" -> redirectTo))), siteContext)
  })

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

    val all = newNavigablePage +: (hiddenPages.filterNot(pagesToRemove.contains) ++ redirectPages)
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


