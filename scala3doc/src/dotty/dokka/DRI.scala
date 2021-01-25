package dotty.dokka

import java.nio.file.Path

val staticFileSymbolUUID = "___staticFile___"

val topLevelDri = DRI("/")

// we may need target...
final case class DRI(
  location: String,
  anchor: String = "",
  origin: String = "",
  symbolUUID: String = ""
):
  def withNoOrigin = copy(origin = "")

  def isStaticFile = symbolUUID == staticFileSymbolUUID

object DRI:
  def forPath(path: Path) = DRI(location = path.toString, symbolUUID = staticFileSymbolUUID)
