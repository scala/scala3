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

case class ClasslikeExtension(parentTypes: List[Bound], constructor: Option[DFunction]) extends ExtraProperty[DClasslike]:
  override def getKey = ClasslikeExtension

object ClasslikeExtension extends BaseKey[DClasslike, ClasslikeExtension]
  

class BaseKey[T, V] extends ExtraProperty.Key[T, V]:
  override def mergeStrategyFor(left: V, right: V): MergeStrategy[T] = 
    MergeStrategy.Remove.INSTANCE.asInstanceOf[MergeStrategy[T]]    