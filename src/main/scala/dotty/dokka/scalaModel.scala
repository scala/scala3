package dotty.dokka

import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.model.properties._  
   
case class MethodExtension(parametersListSizes: Seq[Int]) extends ExtraProperty[DFunction]:
  override def getKey = MethodExtension

object MethodExtension extends BaseKey[DFunction, MethodExtension]

enum Kind(val name: String){
  case Class extends Kind("class")
  case Object extends Kind("object")
  case Trait extends Kind("trait")
}

case class ClasslikeExtension(parentTypes: List[Bound], constructor: Option[DFunction], kind: Kind) extends ExtraProperty[DClasslike]:
  override def getKey = ClasslikeExtension

object ClasslikeExtension extends BaseKey[DClasslike, ClasslikeExtension]
  

case class PropertyExtension(kind: "val" | "var" | "type", isAbstract: Boolean) extends ExtraProperty[DProperty]:
  override def getKey = PropertyExtension

object PropertyExtension extends BaseKey[DProperty, PropertyExtension]

class BaseKey[T, V] extends ExtraProperty.Key[T, V]:
  override def mergeStrategyFor(left: V, right: V): MergeStrategy[T] = 
    MergeStrategy.Remove.INSTANCE.asInstanceOf[MergeStrategy[T]]

enum ScalaOnlyModifiers(val name: String) extends ExtraModifiers(name, null):
  case Implicit extends ScalaOnlyModifiers("implicit")
  case Inline extends ScalaOnlyModifiers("inline")
  case Lazy extends ScalaOnlyModifiers("lazy")
  case Override extends ScalaOnlyModifiers("override")
  case Erased extends ScalaOnlyModifiers("erased")
    
enum ScalaVisibility(val name: String) extends org.jetbrains.dokka.model.Visibility(name, null):
  case NoModifier extends ScalaVisibility("")
  case Protected extends ScalaVisibility("protected")
  case Private extends ScalaVisibility("private")

enum ScalaModifier(val name: String) extends org.jetbrains.dokka.model.Modifier(name, null):
  case Case extends ScalaModifier("case")
  case Abstract extends ScalaModifier("abstract")
  case Final extends ScalaModifier("final")
  case Sealed extends ScalaModifier("sealed")
  case Empty extends ScalaModifier("")