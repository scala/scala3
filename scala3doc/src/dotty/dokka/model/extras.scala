package dotty.dokka

import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.{Projection => JProjection}
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties._
import dotty.dokka.model.api._

case class ModuleExtension(driMap: Map[DRI, Member]) extends ExtraProperty[DModule]:
  override def getKey = ModuleExtension

object ModuleExtension extends BaseKey[DModule, ModuleExtension]

case class MethodExtension(parametersListSizes: Seq[Int]) extends ExtraProperty[DFunction]:
  override def getKey = MethodExtension

object MethodExtension extends BaseKey[DFunction, MethodExtension]

case class ParameterExtension(isExtendedSymbol: Boolean, isGrouped: Boolean) extends ExtraProperty[DParameter]:
  override def getKey = ParameterExtension

object ParameterExtension extends BaseKey[DParameter, ParameterExtension]

case class ClasslikeExtension(
  constructor: Option[DFunction], // will be replaced by signature
  companion: Option[DRI], // moved to kind?
) extends ExtraProperty[DClasslike]:
  override def getKey = ClasslikeExtension

object ClasslikeExtension extends BaseKey[DClasslike, ClasslikeExtension]

case class IsInherited(flag: Boolean) extends ExtraProperty[Documentable]:
  override def getKey = IsInherited

object IsInherited extends BaseKey[Documentable, IsInherited]
