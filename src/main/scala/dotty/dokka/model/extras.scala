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
import java.util.{List => JList, Set => JSet}



case class IsGiven(givenInstance: Option[Bound]) extends ExtraProperty[Documentable]:
  override def getKey = IsGiven

object IsGiven extends BaseKey[Documentable, IsGiven]



case class ExtensionInformation(val isGrouped: Boolean)
   
case class MethodExtension(parametersListSizes: Seq[Int], extensionInfo: Option[ExtensionInformation]) extends ExtraProperty[DFunction]:
  override def getKey = MethodExtension

object MethodExtension extends BaseKey[DFunction, MethodExtension]



case class ParameterExtension(isExtendedSymbol: Boolean, isGrouped: Boolean) extends ExtraProperty[DParameter]:
  override def getKey = ParameterExtension

object ParameterExtension extends BaseKey[DParameter, ParameterExtension]



enum IsEnumEntry extends ExtraProperty[Documentable]:
  case Val
  case Type
  case Class
  override def getKey = IsEnumEntry

object IsEnumEntry extends BaseKey[Documentable, IsEnumEntry]



case class EnumExtension(val enumEntries: Seq[Documentable]) extends ExtraProperty[DClass]:
  override def getKey = EnumExtension

object EnumExtension extends BaseKey[DClass, EnumExtension]



case class ExtensionGroup(val extendedSymbol: DParameter, val extensions: List[DFunction])

enum Kind(val name: String){
  case Class extends Kind("class")
  case Object extends Kind("object")
  case Trait extends Kind("trait")
  case Enum extends Kind("enum")
}

case class ClasslikeExtension(
  parentTypes: List[Bound], 
  constructor: Option[DFunction], 
  kind: Kind, 
  companion: Option[DRI], 
  extensions: List[ExtensionGroup],
  inheritedMethods: List[DFunction],
  givens: List[Documentable]
) extends ExtraProperty[DClasslike]:
  override def getKey = ClasslikeExtension

object ClasslikeExtension extends BaseKey[DClasslike, ClasslikeExtension]

case class SourceLinks(
  links: Map[DokkaConfiguration$DokkaSourceSet, String]
) extends ExtraProperty[Documentable]:
  override def getKey = SourceLinks

object SourceLinks extends BaseKey[Documentable, SourceLinks]

case class InheritanceInfo(
  val parents: List[Bound],
  val knownChildren: List[DRI]
) extends ExtraProperty[DClasslike]:
  override def getKey = InheritanceInfo

object InheritanceInfo extends BaseKey[DClasslike, InheritanceInfo]  

case class PropertyExtension(kind: "val" | "var" | "type", isAbstract: Boolean) extends ExtraProperty[DProperty]:
  override def getKey = PropertyExtension

object PropertyExtension extends BaseKey[DProperty, PropertyExtension]

case class AnnotationsInfo(val annotations: List[AnnotationsInfo.Annotation]) extends ExtraProperty[Documentable]:
    override def getKey = AnnotationsInfo

object AnnotationsInfo extends BaseKey[Documentable, AnnotationsInfo]:
    case class Annotation(val dri: DRI, val params: List[AnnotationParameter])

    sealed trait AnnotationParameter
    case class PrimitiveParameter(val name: Option[String] = None, val value: String) extends AnnotationParameter
    case class LinkParameter(val name: Option[String] = None, val dri: DRI, val value: String) extends AnnotationParameter
    case class UnresolvedParameter(val name: Option[String] = None, val unresolvedText: String) extends AnnotationParameter
