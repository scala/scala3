package dotty.dokka
package site

import org.jetbrains.dokka.base.resolvers.local.DokkaLocationProvider
import org.jetbrains.dokka.base.resolvers.local.LocationProvider
import org.jetbrains.dokka.base.resolvers.local.LocationProviderFactory
import org.jetbrains.dokka.pages.ContentPage
import org.jetbrains.dokka.pages.PageNode
import org.jetbrains.dokka.pages.RootPageNode
import org.jetbrains.dokka.plugability.DokkaContext

import scala.collection.JavaConverters._

class StaticSiteLocationProviderFactory(private val ctx: DokkaContext) extends LocationProviderFactory:
    override def getLocationProvider(pageNode: RootPageNode): LocationProvider =
      new StaticSiteLocationProvider(ctx, pageNode)

class StaticSiteLocationProvider(ctx: DokkaContext, pageNode: RootPageNode) 
  extends DokkaLocationProvider(pageNode, ctx, ".html"):
    private def updatePageEntry(page: PageNode, jpath: JList[String]): JList[String] =
      page match 
        case page: StaticPageNode =>
          if (page.getDri.contains(docsRootDRI)) JList("index")
          else {
            val path = jpath.asScala.toList
            val start = if (path.head == "--root--") List("docs") else path.take(1)
            val pageName = page.template.file.getName
            val dotIndex = pageName.lastIndexOf('.')
            val newName = if (dotIndex < 0) pageName else pageName.substring(0, dotIndex)
            (start ++ path.drop(1).dropRight(1) ++ List(newName)).asJava
          }
        case page: ContentPage if page.getDri.contains(docsDRI) =>
           JList("docs")
        case page: ContentPage if page.getDri.contains(apiPageDRI) =>
          JList("api", "index")
        case _ if jpath.size() > 1 && jpath.get(0) ==   "--root--" && jpath.get(1) == "-a-p-i" =>
          (List("api") ++ jpath.asScala.drop(2)).asJava
        case _ =>
          jpath

    override val getPathsIndex: JMap[PageNode, JList[String]] =
      super.getPathsIndex.asScala.mapValuesInPlace(updatePageEntry).asJava
