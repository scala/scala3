package dotty.dokka
package site

import org.jetbrains.dokka.base.renderers.html.{NavigationNode, NavigationPage}
import org.jetbrains.dokka.model.DPackage
import org.jetbrains.dokka.model.DModule
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.transformers.pages.PageTransformer

import scala.collection.JavaConverters._

class NavigationCreator(ctx: DottyDokkaConfig) extends PageTransformer:

  private def processApiPages(pages: List[PageNode]): JList[NavigationNode] =
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
            val ss = p.getSourceSets.asScala.toSet.toDisplay
            List(new NavigationNode(p.getName, p.getDri, ss, JList())) ++ processChildren
          case _: DModule =>
            processChildren
          case _ =>
            Nil
        case _ =>
          Nil

    pages.flatMap(flatMapPackages).sortBy(_.getName).asJava

  private def processStaticPages(input: PageNode)(staticSiteContext: StaticSiteContext) =
    def toNavigationNode(page: StaticPageNode): NavigationNode = NavigationNode(
      page.title(),
      page.getDri.asScala.head,
      ctx.displaySourceSets,
      page.getChildren.asScala
        .collect { case p: StaticPageNode => toNavigationNode(p)}.asJava
    )

    def singleContentPage(p: PageNode) =
      p.getChildren.asScala.collectFirst { case c: ContentPage => c }.get
    val pageRoot = singleContentPage(input)
    val docsRoot =
      if !pageRoot.getDri.contains(topLevelDri) then pageRoot
      else singleContentPage(pageRoot)
    val apiPages = docsRoot.getChildren.asScala.filterNot(_.isInstanceOf[StaticPageNode])
    val staticPages = staticSiteContext.mainPages.map(toNavigationNode).toList.asJava
    val apiNodes = processApiPages(apiPages.toList)
    staticPages ++
      JList(new NavigationNode("API", apiPageDRI, ctx.displaySourceSets, apiNodes))

  private def emptyNavigationJson =
      val strategy = new RenderingStrategy.Write("[]")
      new RendererSpecificResourcePage("scripts/navigation-pane.json", JList(), strategy)

  final override def invoke(input: RootPageNode): RootPageNode =
    def defaultApiPages = processApiPages(input.getChildren.asScala.toList)
    val nodes = ctx.staticSiteContext.fold(defaultApiPages)(processStaticPages(input))

    val navigationPage = new NavigationPage(new NavigationNode(
      ctx.args.name,
      ctx.staticSiteContext.fold(topLevelDri)(_ => docsRootDRI),
      ctx.displaySourceSets,
      nodes
    ))
    val newChildren = input.getChildren ++ JList(emptyNavigationJson, navigationPage)
    input.modified(input.getName, newChildren)