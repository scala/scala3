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
import java.nio.file.Paths
import java.nio.file.Path

class StaticSiteLocationProviderFactory(private val ctx: DokkaContext) extends LocationProviderFactory:
  override def getLocationProvider(pageNode: RootPageNode): LocationProvider =
    new StaticSiteLocationProvider(ctx, pageNode)

class StaticSiteLocationProvider(ctx: DokkaContext, pageNode: RootPageNode)
  extends DokkaLocationProvider(pageNode, ctx, ".html"):
    private def updatePageEntry(page: PageNode, jpath: JList[String]): JList[String] =
      page match
        case page: StaticPageNode =>
          ctx.siteContext.fold(jpath) { context =>
            val rawFilePath = context.root.toPath.relativize(page.template.file.toPath)
            val pageName = page.template.file.getName
            val dotIndex = pageName.lastIndexOf('.')

            if (isBlogPostPath(rawFilePath)) {
              val regex = raw"(\d*)-(\d*)-(\d*)-(.*)\..*".r
              val blogPostPath = pageName.toString match {
                case regex(year, month, day, name) =>
                  rawFilePath.getParent.resolveSibling(Paths.get(year, month, day, name))
                case _ =>
                  println(s"Blog file at path: $rawFilePath doesn't match desired format.")
                  rawFilePath.resolveSibling(pageName.substring(0, dotIndex))
              }
              blogPostPath.iterator.asScala.map(_.toString).toList.asJava
            } else {
              val newPath =
                if (dotIndex < 0) rawFilePath.resolve("index")
                else rawFilePath.resolveSibling(pageName.substring(0, dotIndex))
              newPath.iterator.asScala.map(_.toString).toList.asJava
            }
          }

        case page: ContentPage if page.getDri.contains(docsDRI) =>
           JList("docs", "index")
        case page: ContentPage if page.getDri.contains(apiPageDRI) =>
          JList("api", "index")
        case _ if jpath.size() > 1 && jpath.get(0) ==   "--root--" && jpath.get(1) == "-a-p-i" =>
          (List("api") ++ jpath.asScala.drop(2)).asJava

        case _: org.jetbrains.dokka.pages.ModulePage if ctx.siteContext.isEmpty =>
          JList("index")
        case _ =>
          jpath

    private def isBlogPostPath(path: Path): Boolean = path.startsWith(Paths.get("blog","_posts"))

    override val getPathsIndex: JMap[PageNode, JList[String]] =
      super.getPathsIndex.asScala.mapValuesInPlace(updatePageEntry).asJava


    override def pathTo(node: PageNode, context: PageNode): String =
      val nodePaths = getPathsIndex.get(node).asScala
      val contextPaths = Option(context).fold(Nil)(getPathsIndex.get(_).asScala.dropRight(1))
      val commonPaths = nodePaths.zip(contextPaths).takeWhile{ case (a, b) => a == b }.size

      val contextPath = contextPaths.drop(commonPaths).map(_ => "..")
      val nodePath = nodePaths.drop(commonPaths) match
          case l if l.isEmpty => Seq("index")
          case l => l
      (contextPath ++ nodePath).mkString("/")