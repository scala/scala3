package dotty.dokka

import org.jetbrains.dokka.model.properties._
import org.jetbrains.dokka.base.signatures._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.base.signatures.KotlinSignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.utilities.DokkaLogger
import collection.JavaConverters._
import org.jetbrains.dokka.base.translators.documentables._
import org.jetbrains.dokka.model.properties.PropertyContainer
import java.util.function.Consumer
import kotlin.jvm.functions.Function2
import java.util.{List => JList, Set => JSet, Map => JMap}
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.plugability._
import kotlin.jvm.JvmClassMappingKt.getKotlinClass

extension [V] (a: WithExtraProperties[_]):
  def get(key: ExtraProperty.Key[_, V]): V = a.getExtra().getMap().get(key).asInstanceOf[V]

extension [E <: WithExtraProperties[E]] (a: E):
  def put(value: ExtraProperty[_ >: E]): E = // TODO remove some of the InstanceOf
  a.withNewExtras(a.getExtra plus value).asInstanceOf[E]

extension [V] (map: JMap[DokkaConfiguration$DokkaSourceSet, V]):
  def defaultValue: V = map.values.asScala.toSeq(0)

extension (sourceSets: Set[DokkaConfiguration$DokkaSourceSet]):
  def toDisplay = sourceSets.map(DisplaySourceSet(_)).asJava

class BaseKey[T, V] extends ExtraProperty.Key[T, V]:
  override def mergeStrategyFor(left: V, right: V): MergeStrategy[T] =
  MergeStrategy.Remove.INSTANCE.asInstanceOf[MergeStrategy[T]]

  def definedIn(e: T): Boolean = e match
  case e: WithExtraProperties[_] => e.getExtra.getMap.containsKey(this)
  case _ => false


  def getFrom(e: T): Option[V] = e match
  case e: WithExtraProperties[_] => getFromExtra(e, this)
  case _ => None

def getFromExtra[V](e: WithExtraProperties[_], k: ExtraProperty.Key[_, V]): Option[V] =
  Option(e.getExtra.getMap.get(k)).asInstanceOf[Option[V]]


extension (f: DFunction):
  def isRightAssociative(): Boolean = f.getName.endsWith(":")

object JList:
  def apply[T](elem: T): JList[T] = List(elem).asJava
  def apply[T]() = List[T]().asJava

object JSet:
  def apply[T](elem: T): JSet[T] = Set(elem).asJava
  def apply[T]() = Set[T]().asJava

def modifyContentGroup(originalContentNodeWithParents: Seq[ContentGroup], modifiedContentNode: ContentGroup): ContentGroup =
  originalContentNodeWithParents match {
    case head :: tail => tail match {
      case tailHead :: tailTail =>
        val newChildren = tailHead.getChildren.asScala.map(c => if c != head then c else modifiedContentNode)
        modifyContentGroup(
          tailTail,
          tailHead.copy(
            newChildren.asJava,
            tailHead.getDci,
            tailHead.getSourceSets,
            tailHead.getStyle,
            tailHead.getExtra
          )
        )
      case _ => head
    }
    case _ => modifiedContentNode
  }

def getContentGroupWithParents(root: ContentGroup, condition: ContentGroup => Boolean): Seq[ContentGroup] = {
  def getFirstMatch(list: List[ContentNode]): Seq[ContentGroup] = list match {
    case head :: tail => head match {
      case g: ContentGroup =>
        val res = getContentGroupWithParents(g, condition)
        if(!res.isEmpty) res
        else getFirstMatch(tail)
      case _ => getFirstMatch(tail)
    }

    case _ => Seq()
  }
  if(condition(root)) Seq(root)
  else {
    val res = getFirstMatch(root.getChildren.asScala.toList)
    if(!res.isEmpty) res ++ Seq(root)
    else Seq()
  }
}
