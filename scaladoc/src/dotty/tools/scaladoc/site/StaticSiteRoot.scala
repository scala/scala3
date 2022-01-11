package dotty.tools.scaladoc
package site

import java.nio.file.Path

case class StaticSiteRoot(
  rootTemplate: LoadedTemplate,
  siteMappings: Map[Path, Path]
):
  lazy val reverseSiteMappings = siteMappings.map(_.swap).toMap
  lazy val sources = siteMappings.keys.toSet
  lazy val dests = reverseSiteMappings.keys.toSet