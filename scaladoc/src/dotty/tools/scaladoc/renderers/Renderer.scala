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

case class Page(link: Link, content: Member | ResolvedTemplate | String, children: Seq[Page], hidden: Boolean = false):
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

  val rootApiPage: Option[Page] = Some(memberPage(rootPackage)).filter(_.children.nonEmpty).map(_.withTitle(ctx.args.name))

  val rootDocsPage: Option[Page] = staticSite match
      case None => None
      case Some(siteContext) =>
        val rootTemplate = siteContext.staticSiteRoot.rootTemplate

        // Below code is for walking in order the tree and modifing its nodes basing on its neighbours

        // We add dummy guards
        val allTemplates: Seq[Option[LoadedTemplate]] = None +: siteContext.allTemplates.map(Some(_)) :+ None

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

        val newTemplates = updateSettings(Seq(rootTemplate), newSettings.to(ListBuffer))
        val templatePages = newTemplates.map(templateToPage(_, siteContext))

        val newRoot = newTemplates.head

        Some(newRoot).filter(r => r.children.nonEmpty || r.templateFile.rawCode.nonEmpty)
          .map(templateToPage(_, siteContext))

  val redirectPages: Seq[Page] = staticSite.fold(Seq.empty)(siteContext => siteContext.redirectTemplates.map {
    case (template, driFrom, driTo) =>
      val redirectTo = pathToPage(driFrom, driTo)
      templateToPage(template.copy(templateFile = template.templateFile.copy(settings = template.templateFile.settings ++ Map("redirectTo" -> redirectTo))), siteContext)
  })

  /**
   * Here we have to retrive index pages from hidden pages and replace fake index pages in navigable page tree.
   */
  val allPages: Seq[Page] =
    val all = rootApiPage ++ rootDocsPage ++ redirectPages
    // We need to check for conflicts only if we have top-level member called docs
    val hasPotentialConflict =
      rootPackage.members.exists(m => m.name.startsWith("_docs"))

    if hasPotentialConflict then
      def walk(page: Page): Unit =
        if page.link.dri.isStaticFile then
          val dest = absolutePath(page.link.dri)
          if apiPaths.contains(dest) then
            report.error(s"Conflict between static page and API member for $dest. $pathsConflictResoultionMsg")
          page.children.foreach(walk)

      all.foreach(walk)

    all.toSeq

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


