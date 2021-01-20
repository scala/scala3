package dotty.dokka

import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import collection.JavaConverters._
import org.jetbrains.dokka.model.DisplaySourceSet
import org.jetbrains.dokka.model.properties.WithExtraProperties
// TODO reproduction! - comment line below to broke compiler!
import org.jetbrains.dokka.model.properties.ExtraProperty
// import java.util.Stream // TODO reproduction uncomment
import java.util.stream.Stream // comment out - wrong error!
import java.util.stream.Collectors
import java.util.Collections
import org.jetbrains.dokka.plugability._
import kotlin.jvm.JvmClassMappingKt.getKotlinClass
import org.jetbrains.dokka.links._
import java.nio.file.Path
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.model.Documentable

val U: kotlin.Unit = kotlin.Unit.INSTANCE

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

type SourceSetWrapper = DokkaConfiguration$DokkaSourceSet
type DokkaSourceSet = DokkaConfiguration.DokkaSourceSet

type DocPart = org.jetbrains.dokka.model.doc.DocTag

extension [T] (wrapper: SourceSetWrapper)
    def toSet: JSet[DokkaConfiguration$DokkaSourceSet] = JSet(wrapper)
    def toMap(value: T): JMap[DokkaConfiguration$DokkaSourceSet, T] = JMap(wrapper -> value)

extension [T] (wrapper: DokkaSourceSet)
    // when named `toSet` fails in runtime -- TODO: create a minimal!
    // def toSet: JSet[DokkaConfiguration$DokkaSourceSet] = JSet(wrapper.asInstanceOf[SourceSetWrapper])
    def asSet: JSet[DokkaConfiguration$DokkaSourceSet] = JSet(wrapper.asInstanceOf[SourceSetWrapper])
    def asMap(value: T): JMap[DokkaConfiguration$DokkaSourceSet, T] = JMap(wrapper.asInstanceOf[SourceSetWrapper] -> value)

extension (sourceSets: JList[DokkaSourceSet])
  def asDokka: JSet[SourceSetWrapper] = sourceSets.asScala.toSet.asJava.asInstanceOf[JSet[SourceSetWrapper]]
  def toDisplaySourceSet = sourceSets.asScala.map(ss => DisplaySourceSet(ss.asInstanceOf[SourceSetWrapper])).toSet.asJava

extension (sourceSets: Set[SourceSetWrapper])
  def toDisplay = sourceSets.map(DisplaySourceSet(_)).asJava

extension [V] (a: WithExtraProperties[_])
  def get(key: ExtraProperty.Key[_, V]): V = a.getExtra().getMap().get(key).asInstanceOf[V]

extension [E <: WithExtraProperties[E]] (a: E)
  def put(value: ExtraProperty[_ >: E]): E = a.withNewExtras(a.getExtra plus value)

extension [V] (map: JMap[SourceSetWrapper, V])
  def defaultValue: V = map.values.asScala.head

extension [V](jlist: JList[V])
  def ++ (other: JList[V]): JList[V] =
    Stream.of(jlist, other).flatMap(_.stream).collect(Collectors.toList())

extension [V](jset: JSet[V])
  def ++ (other: JSet[V]): JSet[V] =
    Stream.of(jset, other).flatMap(_.stream).collect(Collectors.toSet())

// Needed until we will migrate away from dokka


case class FakeContentPage(
  dri: DRI,
  override val getContent: ContentNode) extends ContentPage:
  override val getName: String = ""
  override val getChildren: JList[PageNode] = JList()
  override val getEmbeddedResources: JList[String] = JList()
  override def getDocumentable: Documentable = null
  override def modified(
    name: String,
    content: ContentNode,
    dri: JSet[DRI],
    embeddedResources: JList[String],
    children: JList[_ <: PageNode]
  ): ContentPage = this
  override def modified(name: String, children: JList[_ <: PageNode]): PageNode = this
  override val getDri: JSet[DRI] = JSet(dri)
