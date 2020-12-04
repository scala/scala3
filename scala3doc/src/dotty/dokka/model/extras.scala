package dotty.dokka

import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.model.{Projection => JProjection}
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties._
import dotty.dokka.model.api._

case class ModuleExtension(driMap: Map[DRI, Member]) extends ExtraProperty[DModule]:
  override def getKey = ModuleExtension

object ModuleExtension extends BaseKey[DModule, ModuleExtension]
