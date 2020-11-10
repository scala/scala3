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
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet

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
