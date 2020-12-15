package dotty.dokka
package site

import org.jetbrains.dokka.pages.ContentPage
import org.jetbrains.dokka.pages.PageNode
import org.jetbrains.dokka.pages.RootPageNode
import org.jetbrains.dokka.pages.ModulePage
import org.jetbrains.dokka.pages.ClasslikePageNode
import org.jetbrains.dokka.model.DPackage
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.base.resolvers.external._
import org.jetbrains.dokka.base.resolvers.shared._
import org.jetbrains.dokka.base.resolvers.local._
import org.jetbrains.dokka.model.DisplaySourceSet
import dotty.dokka.withNoOrigin

import scala.collection.JavaConverters._
import java.nio.file.Paths
import java.nio.file.Path
import scala.util.matching._
import dotty.dokka.model.api._

class StaticSiteLocationProviderFactory(using ctx: DokkaContext) extends LocationProviderFactory:
  override def getLocationProvider(pageNode: RootPageNode): LocationProvider =
    try new StaticSiteLocationProvider(pageNode)
    catch
      case e: Error =>
        // TODO (https://github.com/lampepfl/scala3doc/issues/238) error handling
        e.printStackTrace()
        // We encounter bug in Kotlin coroutines (race) when this method throws exception
        // In such case we want to return null to trigger NPE in other piece of code to fail properly coroutine context
        // Making generated DRIs not-unique will reproduce this behavior
        null

class StaticSiteLocationProvider(pageNode: RootPageNode)(using ctx: DokkaContext)
  extends DokkaLocationProvider(pageNode, ctx, ".html"):
    private def updatePageEntry(page: PageNode, jpath: JList[String]): JList[String] =
      page match
        case page: ContentPage if page.getDri.contains(docsDRI) =>
           JList("docs", "index")
        case page: ContentPage if page.getDri.contains(docsRootDRI) =>
           JList("index")
        case page: StaticPageNode =>
          summon[DocContext].staticSiteContext.fold(jpath) { context =>
            val rawFilePath = context.root.toPath.relativize(page.template.file.toPath)
            val pageName = page.template.file.getName
            val dotIndex = pageName.lastIndexOf('.')

            if (isBlogPostPath(rawFilePath)) {
              val regex = raw"(\d*)-(\d*)-(\d*)-(.*)\..*".r
              val blogPostPath = pageName.toString match {
                case regex(year, month, day, name) =>
                  rawFilePath.getParent.resolveSibling(Paths.get(year, month, day, name))
                case _ =>
                  val msg = s"Relative path for blog: $rawFilePath doesn't match `yyy-mm-dd-name.md` format."
                  report.warn(msg, page.template.file)
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

        case page: ContentPage if page.getDri.contains(apiPageDRI) =>
          JList("api", "index")
        case _ if jpath.size() > 1 && jpath.get(0) ==   "--root--" && jpath.get(1) == "-a-p-i" =>
          (List("api") ++ jpath.asScala.drop(2)).asJava

        case _: ModulePage if summon[DocContext].staticSiteContext.isEmpty =>
          JList("index")
        case page: ContentPage if page.getDocumentable != null =>
          (
            List("api") ++
            page.getDocumentable.getDri.location.split(Array('.')).toList ++
            (if(page.getDocumentable.isInstanceOf[DPackage]) then List("index") else List.empty) ++
            page.getDocumentable.getDri.anchor
          ).asJava
        case _ =>
          jpath

    private def isBlogPostPath(path: Path): Boolean = path.startsWith(Paths.get("blog","_posts"))

    override val getPathsIndex: JMap[PageNode, JList[String]] =
      super.getPathsIndex.asScala.mapValuesInPlace(updatePageEntry).asJava

    // We should build our own provider at some point
    val ourPages: Map[String, ClasslikePageNode] = getPathsIndex.asScala.collect {
        case (node: ClasslikePageNode, path) => node.getDri.asScala.head.location -> node
      }.toMap


    override def resolve(
      dri: DRI,
      sourceSets: JSet[DisplaySourceSet],
      context: PageNode): String =
        ourPages.get(dri.location).fold(super.resolve(dri, sourceSets, context)){ page =>
          val path = pathTo(page,context) match
            case "" => ""
            case path => s"$path.html"
          dri.anchor.fold(path)(hash => s"$path#$hash")
        }

    override def resolve(node: PageNode, from: PageNode, skipExtension: Boolean): String =
      pathTo(node, from) match
        case "" => ""
        case path => if skipExtension then path else s"$path.html"

    override def pathTo(node: PageNode, context: PageNode): String =
      if node == context then ""
      else
        val nodePaths = getPathsIndex.get(node).asScala.toList
        val contextPaths = Option(context).fold(Nil)(getPathsIndex.get(_).asScala).toList
        relativePath(contextPaths, nodePaths)


    val externalLocationProviders: List[(List[Regex], ExternalLocationProvider)] =
      val sourceSet = ctx.getConfiguration.getSourceSets.asScala(0)
      ctx.getConfiguration
        .asInstanceOf[DocContext]
        .externalDocumentationLinks
        .map { link =>
          val emptyExtDoc = ExternalDocumentation(
            link.documentationUrl,
            PackageList(
              RecognizedLinkFormat.Javadoc1, JSet(), JMap(), link.documentationUrl
            )
          )
          val extDoc = link.packageListUrl.fold(emptyExtDoc)( pl => ExternalDocumentation(
              link.documentationUrl,
              PackageList.Companion.load(pl, sourceSet.getJdkVersion, ctx.getConfiguration.getOfflineMode)
            )
          )
          (extDoc, link)
        }
        .map { (extDoc, link) =>

          val externalLocationProvider = ScalaExternalLocationProvider(extDoc, ".html", link.kind)
          link.originRegexes -> externalLocationProvider
        }.toList

    override def getExternalLocation(dri: DRI, sourceSets: JSet[DisplaySourceSet]): String =
      val regex = raw"\[origin:(.*)\]".r
      val origin = regex.findFirstIn(Option(dri.extra).getOrElse(""))
      origin match {
        case Some(path) => externalLocationProviders.find { (regexes, provider) =>
          regexes.exists(r => r.matches(path))
        }.fold(null)(_(1).resolve(dri.withNoOrigin))
        case None => null
      }

def relativePath(fullFrom: Seq[String], to: Seq[String]): String =
  val from = fullFrom.dropRight(1)
  val commonPaths = to.zip(from).takeWhile{ case (a, b) => a == b }.size

  val contextPath = from.drop(commonPaths).map(_ => "..")
  val nodePath = to.drop(commonPaths) match
      case Nil if contextPath.isEmpty && to.nonEmpty=> Seq("..", to.last)
      case Nil => Seq("index")
      case l => l
  (contextPath ++ nodePath).mkString("/")