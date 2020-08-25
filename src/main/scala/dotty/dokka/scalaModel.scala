package dotty.dokka

import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties._  

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

enum Kind(val name: String){
  case Class extends Kind("class")
  case Object extends Kind("object")
  case Trait extends Kind("trait")
  case Enum extends Kind("enum")
}

case class ExtensionGroup(val extendedSymbol: DParameter, val extensions: List[DFunction])

case class EnumExtension(val enumEntries: Seq[Documentable]) extends ExtraProperty[DClass]:
  override def getKey = EnumExtension

object EnumExtension extends BaseKey[DClass, EnumExtension]

case class TastyDocumentableSource(val path: String, val lineNumber: Int) extends DocumentableSource {
    override def getPath = path
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

case class InheritanceInfo(
  val parents: List[Bound],
  val knownChildren: List[DRI]
) extends ExtraProperty[DClasslike]:
  override def getKey = InheritanceInfo

object InheritanceInfo extends BaseKey[DClasslike, InheritanceInfo]  

case class PropertyExtension(kind: "val" | "var" | "type", isAbstract: Boolean) extends ExtraProperty[DProperty]:
  override def getKey = PropertyExtension

object PropertyExtension extends BaseKey[DProperty, PropertyExtension]

class BaseKey[T, V] extends ExtraProperty.Key[T, V]:
  override def mergeStrategyFor(left: V, right: V): MergeStrategy[T] = 
    MergeStrategy.Remove.INSTANCE.asInstanceOf[MergeStrategy[T]]

enum ScalaOnlyModifiers(val name: String, val prefix: Boolean) extends ExtraModifiers(name, null):
  case Sealed extends ScalaOnlyModifiers("sealed", true)
  case Case extends ScalaOnlyModifiers("case", false)
  case Implicit extends ScalaOnlyModifiers("implicit", true)
  case Inline extends ScalaOnlyModifiers("inline", true)
  case Lazy extends ScalaOnlyModifiers("lazy", true)
  case Override extends ScalaOnlyModifiers("override", true)
  case Erased extends ScalaOnlyModifiers("erased", true)
  case Opaque extends ScalaOnlyModifiers("opaque", true)
    
enum ScalaVisibility(val name: String) extends org.jetbrains.dokka.model.Visibility(name, null):
  case NoModifier extends ScalaVisibility("")
  case Protected extends ScalaVisibility("protected")
  case Private extends ScalaVisibility("private")

enum ScalaModifier(val name: String) extends org.jetbrains.dokka.model.Modifier(name, null):
  case Abstract extends ScalaModifier("abstract")
  case Final extends ScalaModifier("final")
  case Empty extends ScalaModifier("")

extension (f: DFunction):
  def isRightAssociative(): Boolean = f.getName.endsWith(":")
  def getExtendedSymbol(): Option[DParameter] = Option.when(f.get(MethodExtension).extensionInfo.isDefined)(
    f.getParameters.asScala(if f.isRightAssociative() then f.get(MethodExtension).parametersListSizes(0) else 0)
  )
