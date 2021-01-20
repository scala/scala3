package dotty.dokka

import java.nio.file.Path
import org.jetbrains.dokka.links.PointingToDeclaration

val staticFileSymbolUUID = "___staticFile___"

val topLevelDri = DRI("/")

type DDRI = org.jetbrains.dokka.links.DRI

// we may need target...
case class DRI(
  location: String,
  anchor: String = "",
  origin: String = "",
  symbolUUID: String = ""
):
  def withNoOrigin = copy(origin = "")

  def isStaticFile = symbolUUID == staticFileSymbolUUID

  def asDokka: DDRI = new DDRI(
    location,
    anchor,
    null,
    PointingToDeclaration.INSTANCE,
    origin + ":" + symbolUUID
  )

object DRI:
  def forPath(path: Path) = DRI(location = path.toString, symbolUUID = staticFileSymbolUUID)

extension (dokkaDri: DDRI)
  def asScala: DRI =
    val elements = dokkaDri.getExtra.split(":")
    val origin = elements.headOption.getOrElse("")
    val symbolUUID = elements.drop(1).mkString(":")
    DRI(
      dokkaDri.getPackageName,
      dokkaDri.getClassNames,
      origin,
      symbolUUID
    )