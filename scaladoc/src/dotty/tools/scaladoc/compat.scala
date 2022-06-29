package dotty.tools.scaladoc

import java.util.stream.Stream // comment out - wrong error!
import java.util.stream.Collectors
import java.util.Collections
import java.nio.file.Path
import com.vladsch.flexmark.util.ast.{Node => MdNode}
import dotty.tools.scaladoc.tasty.comments.wiki.WikiDocElement
import scala.jdk.CollectionConverters._

def JList[T](e: T*): JList[T] = e.asJava
def JSet[T](e: T*): JSet[T] = e.toSet.asJava
def JMap[K, V](e: (K, V)*): JMap[K, V] = e.toMap.asJava

type JList[T] = java.util.List[T]
type JSet[T] = java.util.Set[T]
type JMap[K, V] = java.util.Map[K, V]
type JHashMap[K, V] = java.util.HashMap[K, V]
type JMapEntry[K, V] = java.util.Map.Entry[K, V]

private val emptyListInst = Collections.emptyList
def JNil[A] = emptyListInst.asInstanceOf[JList[A]]

private val emptyMapInst = Collections.emptyMap
def emptyJMap[A, B] = emptyMapInst.asInstanceOf[JMap[A, B]]

enum DocLink:
  case ToURL(url: String)
  case ToDRI(dri: DRI, name: String)
  case UnresolvedDRI(query: String, msg: String)

type DocPart = Seq[WikiDocElement] | MdNode

extension [V](jlist: JList[V])
  def ++ (other: JList[V]): JList[V] =
    Stream.of(jlist, other).flatMap(_.stream).collect(Collectors.toList())

extension [V](jset: JSet[V])
  def ++ (other: JSet[V]): JSet[V] =
    Stream.of(jset, other).flatMap(_.stream).collect(Collectors.toSet())