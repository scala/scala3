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
import java.io.File
import scala.util.matching._
import dotty.tools.scaladoc.util.Escape._

val UnresolvedLocationLink = "#"

trait Locations(using ctx: DocContext):
  def effectiveMembers: Map[DRI, Member]

  // We generate this collection only if there may be a conflict with resources.
  // Potentially can be quite big.
  lazy val apiPaths = effectiveMembers.keySet.filterNot(_.isStaticFile).map(absolutePath(_))

  var cache = new JHashMap[DRI, Seq[String]]()

  private[renderers] def pathsConflictResoultionMsg =
    "Using `-Yapi-subdirectory` flag will move all API documentation into `api` subdirectory and will fix this conflict."

  // TODO verify if location exisits
  def rawLocation(dri: DRI): Seq[String] =
    cache.get(dri) match
      case null =>
        val path = dri match
          case `apiPageDRI` =>
            if ctx.args.apiSubdirectory && ctx.staticSiteContext.nonEmpty
              then List("api", "index")
              else List("index")
          case dri if dri.isStaticFile =>
            Paths.get(dri.location).iterator.asScala.map(_.toString).toList
          case dri =>
            val loc = dri.location
            val fqn = loc.split(Array('.')).toList match
              case "<empty>" :: Nil  => "_empty_" :: Nil
              case "<empty>" :: tail => "_empty_" :: tail
              case other => other
            if ctx.args.apiSubdirectory then "api" :: fqn else fqn
        cache.put(dri, path)
        path
      case cached => cached

  private def unknownPage(dri: DRI): String =
    // TODO we should switch that to warning probably or has dedicated setting
    report.inform(s"Unrecognized page for ${dri.location} ($dri)")
    UnresolvedLocationLink

  def pathToPage(from: DRI, to: DRI): String =
    if to.isStaticFile || effectiveMembers.contains(to) then
      val anchor = if to.anchor.isEmpty then "" else "#" + to.anchor
      pathToRaw(rawLocation(from), rawLocation(to)) +".html" + anchor
    else
      to.externalLink.fold(unknownPage(to))(l => l)

  def pathToRaw(from: Seq[String], to: Seq[String]): String =
    import dotty.tools.scaladoc.util.Escape._
    val fromDir = from.dropRight(1)
    val commonPaths = to.zip(fromDir).takeWhile{ case (a, b) => a == b }.size

    val contextPath = fromDir.drop(commonPaths).map(_ => "..")
    val nodePath = to.drop(commonPaths) match
        case Nil if contextPath.isEmpty && to.nonEmpty=> Seq("..", to.last)
        case Nil => to.lastOption.fold(Seq("index"))(".." :: _ :: Nil)
        case l => l

    escapeUrl((contextPath ++ nodePath).mkString("/"))

  def resolveRoot(from: Seq[String], to: String): String =
    pathToRaw(from, to.split("/").toList)

  def resolveRoot(dri: DRI, path: String): String = resolveRoot(rawLocation(dri), path)
  def absolutePath(dri: DRI, extension: String = "html"): String = rawLocation(dri).mkString("", "/", s".$extension")

  def resolveLink(dri: DRI, url: String): String =
    if URI(url).isAbsolute then url else resolveRoot(dri, url)

  def pathToRoot(dri: DRI): String = rawLocation(dri).drop(1).map(_ => "..") match
    case Nil => ""
    case seq => seq.mkString("", "/" , "/")

  def driExists(dri: DRI) = effectiveMembers.get(dri).isDefined || dri.isStaticFile
