package dotty.dokka

import org.jetbrains.dokka.base.renderers._
import org.jetbrains.dokka.base.resolvers.local._
import org.jetbrains.dokka.base._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.plugability._
import collection.JavaConverters._
import dotty.dokka.model.api.withNoOrigin

object ScalaPackageListService:
  val DOKKA_PARAM_PREFIX = "$dokka"

class ScalaPackageListService(context: DokkaContext, rootPage: RootPageNode):
  import ScalaPackageListService._  //Why I need to do this?

  val locationProvider = PluginUtils.querySingle[DokkaBase, LocationProviderFactory](context, _.getLocationProviderFactory)
    .getLocationProvider(rootPage)

  def createPackageList(format: String, linkExtension: String): String = {
    val packages = retrievePackageInfo(rootPage)
    val relocations = getRelocations(rootPage)
    s"$DOKKA_PARAM_PREFIX.format:$format\n" ++
    s"$DOKKA_PARAM_PREFIX.linkExtenstion:$linkExtension\n" ++
    relocations.map( (dri, link) =>
      s"$DOKKA_PARAM_PREFIX.location:${dri.withNoOrigin.toString}\u001f$link.$linkExtension"
    ).mkString("","\n","\n") ++
    packages.mkString("","\n","\n")
  }

  private def retrievePackageInfo(current: PageNode): Set[String] = current match {
    case p: PackagePageNode => p.getChildren.asScala.toSet.flatMap(retrievePackageInfo) ++ Option(p.getDocumentable.getDri.getPackageName)
    case other => other.getChildren.asScala.toSet.flatMap(retrievePackageInfo)
  }

  private def getRelocations(current: PageNode): List[(DRI, String)] = current match {
    case c: ContentPage => getRelocation(c.getDri.asScala.toList, c) ++ c.getChildren.asScala.toList.flatMap(getRelocations)
    case other => other.getChildren.asScala.toList.flatMap(getRelocations)
  }

  private def getRelocation(dris: List[DRI], node: ContentPage): List[(DRI, String)] =
    val link = locationProvider.resolve(node, rootPage, true)
    dris.map( dri =>
      if locationProvider.expectedLocationForDri(dri) != link then Some(dri, link) else None
    ).flatten