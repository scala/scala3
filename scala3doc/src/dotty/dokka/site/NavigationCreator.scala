package dotty.dokka
package site

import org.jetbrains.dokka.model.DPackage
import org.jetbrains.dokka.model.DModule
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.transformers.pages.PageTransformer

import scala.collection.JavaConverters._

class NavigationCreator(using ctx: DocContext) extends PageTransformer:

  private def processApiPages(pages: List[PageNode]): List[NavigationNode] =
    def flatMapPackages(pn: PageNode): List[NavigationNode] =
      def processChildren = pn.getChildren.asScala.flatMap(flatMapPackages).toList
      pn match
        case cp: ContentPage => cp.getDocumentable match
          case null =>
            processChildren
          // Left over package from dokka
          case p: DPackage if p.getName == "<empty>" && p.getChildren.isEmpty =>
            Nil
          case p: DPackage =>
            List(new NavigationNode(p.getName, p.getDri, Nil)) ++ processChildren
          case _: DModule =>
            processChildren
          case _ =>
            Nil
        case _ =>
          Nil

    pages.flatMap(flatMapPackages).sortBy(_.name)

  private def processStaticPages(input: PageNode)(staticSiteContext: StaticSiteContext) =
    def toNavigationNode(page: StaticPageNode): NavigationNode = NavigationNode(
      page.title(),
      page.getDri.asScala.head,
      page.getChildren.asScala.collect { case p: StaticPageNode => toNavigationNode(p) }.toList
    )

    def singleContentPage(p: PageNode) =
      p.getChildren.asScala.collectFirst { case c: ContentPage => c }.get
    val pageRoot = singleContentPage(input)
    val docsRoot =
      if !pageRoot.getDri.contains(topLevelDri) then pageRoot
      else singleContentPage(pageRoot)
    val apiPages = docsRoot.getChildren.asScala.filterNot(_.isInstanceOf[StaticPageNode])
    val staticPages = staticSiteContext.mainPages.map(toNavigationNode).toList
    val apiNodes = processApiPages(apiPages.toList)
    staticPages ++ List(NavigationNode("API", apiPageDRI, apiNodes))

  private def emptyNavigationJson =
      val strategy = new RenderingStrategy.Write("[]")
      new RendererSpecificResourcePage("scripts/navigation-pane.json", JList(), strategy)

  final override def invoke(input: RootPageNode): RootPageNode =
    def defaultApiPages = processApiPages(input.getChildren.asScala.toList)
    val nodes = ctx.staticSiteContext.fold(defaultApiPages)(processStaticPages(input))

    summon[DocContext].navigationNode = Some(NavigationNode(
      ctx.args.name,
      ctx.staticSiteContext.fold(topLevelDri)(_ => docsRootDRI),
      nodes
    ))

    val newChildren = input.getChildren ++ JList(emptyNavigationJson)
    input.modified(input.getName, newChildren)