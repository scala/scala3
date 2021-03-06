package dotty.tools.scaladoc

import java.nio.file.Path
import dotty.tools.scaladoc.util.Escape._

val staticFileSymbolUUID = "___staticFile___"

val topLevelDri = DRI("/")

// we may need target...
final case class DRI(
  location: String,
  anchor: String = "",
  externalLink: Option[String] = None,
  symbolUUID: String = ""
):
  def withNoExternalLink = copy(externalLink = None)

  def isStaticFile = symbolUUID == staticFileSymbolUUID

  def asFileLocation: String = escapeUrl(location).replace(".", "/")

object DRI:
  def forPath(path: Path) =
    DRI(location = path.toString, symbolUUID = staticFileSymbolUUID)
