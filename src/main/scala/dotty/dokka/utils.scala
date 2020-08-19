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
import dokka.java.api._
import java.util.function.Consumer
import kotlin.jvm.functions.Function2
import java.util.{List => JList, Set => JSet, Map => JMap}
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet

extension  on[T, V] (a: WithExtraProperties[T]):
  def get(key: ExtraProperty.Key[_ >: T, V]): V = a.getExtra().getMap().get(key).asInstanceOf[V]

extension on[V] (map: JMap[DokkaConfiguration$DokkaSourceSet, V]):
    def defaultValue: V = map.values.asScala.toSeq(0)

extension on (builder: PageContentBuilder):
    def contentFor(d: Documentable)(
        func: Function1[PageContentBuilder#DocumentableContentBuilder, kotlin.Unit],
        kind: ContentKind = ContentKind.Main,
        styles: Set[Style] = Set(),
        extras: PropertyContainer[ContentNode] = PropertyContainer.Companion.empty(),
        sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = d.getSourceSets.asScala.toSet
    ): ContentGroup = builder.contentFor(d, kind, styles.asJava, extras, sourceSets.asJava, a => func(a))

/* Need to do it this way, because Dotty seems to not support named parameters of extensions and I couldn't change kind */
def group(builder: PageContentBuilder#DocumentableContentBuilder)(
    func: Function1[PageContentBuilder#DocumentableContentBuilder, kotlin.Unit],
    dri: Set[DRI] = builder.getMainDRI.asScala.toSet,
    sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = builder.getMainSourcesetData.asScala.toSet,
    kind: Kind = ContentKind.Main,
    styles: Set[Style] = builder.getMainStyles.asScala.toSet,
    extra: PropertyContainer[ContentNode] = builder.getMainExtra
): Unit = builder.group(dri.asJava, sourceSets.asJava, kind, styles.asJava, extra, a => func(a))

def sourceSetDependentHint(builder: PageContentBuilder#DocumentableContentBuilder)(
    func: Function1[PageContentBuilder#DocumentableContentBuilder, kotlin.Unit],
    dri: JSet[DRI] = builder.getMainDRI,
    sourceSets: JSet[DokkaConfiguration$DokkaSourceSet] = builder.getMainSourcesetData,
    kind: Kind = ContentKind.Main,
    styles: JSet[Style] = builder.getMainStyles,
    extra: PropertyContainer[ContentNode] = builder.getMainExtra
): Unit = builder.sourceSetDependentHint(dri, sourceSets, kind, styles, extra, a => func(a))

def header(builder: PageContentBuilder#DocumentableContentBuilder, level: Int, text: String)(
    func: Function1[PageContentBuilder#DocumentableContentBuilder, kotlin.Unit] = builder => kotlin.Unit.INSTANCE,
    dri: JSet[DRI] = builder.getMainDRI,
    sourceSets: JSet[DokkaConfiguration$DokkaSourceSet] = builder.getMainSourcesetData,
    kind: Kind = ContentKind.Main,
    styles: JSet[Style] = builder.getMainStyles,
    extra: PropertyContainer[ContentNode] = builder.getMainExtra
): Unit = builder.header(level, text, kind, sourceSets, styles, extra, a => func(a))

def text(builder: PageContentBuilder#DocumentableContentBuilder, text: String)(
    sourceSets: JSet[DokkaConfiguration$DokkaSourceSet] = builder.getMainSourcesetData,
    kind: Kind = ContentKind.Main,
    styles: JSet[Style] = builder.getMainStyles,
    extra: PropertyContainer[ContentNode] = builder.getMainExtra
) = builder.text(text, kind, sourceSets, styles, extra)

def divergentGroup(
    builder: PageContentBuilder#DocumentableContentBuilder,
    groupId: ContentDivergentGroup$GroupID
)(
    func: Function1[PageContentBuilder$DivergentBuilder, kotlin.Unit],
    dri: Set[DRI] = builder.getMainDRI.asScala.toSet,
    kind: Kind = ContentKind.Main,
    styles: Set[Style] = builder.getMainStyles.asScala.toSet,
    extra: PropertyContainer[ContentNode] = builder.getMainExtra,
    implicitlySourceSetHinted: Boolean = true,
) = builder.divergentGroup(
    groupId,
    dri.asJava,
    kind,
    styles.asJava,
    extra,
    implicitlySourceSetHinted,
    a => func(a)
)

def link(builder: PageContentBuilder#DocumentableContentBuilder, text: String, address: DRI)(
    kind: Kind = ContentKind.Main,
    sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = builder.getMainSourcesetData.asScala.toSet,
    styles: Set[Style] = builder.getMainStyles.asScala.toSet,
    extra: PropertyContainer[ContentNode] = builder.getMainExtra
) = {
    builder.unaryPlus(linkNode(builder, text, address)(sourceSets = sourceSets, styles = styles, extra = extra))
}

def linkNode(
    builder: PageContentBuilder#DocumentableContentBuilder,
    text: String, 
    address: DRI
)(
    kind: Kind = ContentKind.Main,
    sourceSets: Set[DokkaConfiguration$DokkaSourceSet] = builder.getMainSourcesetData.asScala.toSet,
    styles: Set[Style] = builder.getMainStyles.asScala.toSet,
    extra: PropertyContainer[ContentNode] = builder.getMainExtra
) = ContentDRILink(
        List(ContentText(text, DCI(builder.getMainDRI, kind), sourceSets.asJava, styles.asJava, extra)).asJava,
        address,
        DCI(builder.getMainDRI, kind),
        sourceSets.asJava,
        styles.asJava,
        extra
    )

extension on(builder: PageContentBuilder$DocumentableContentBuilder):

    def addText(str: String) = builder.text(str, ContentKind.Main, builder.getMainSourcesetData, builder.getMainStyles, builder.getMainExtra) 
    
    def addList[T](
        elements: JList[T],
        prefix: String = "",
        suffix: String = "",
        separator: String = ", "
    )(op: T => Unit): Unit = 
        val lambda: Function2[Any, Any, kotlin.Unit] = new Function2[Any,Any, kotlin.Unit]:
            def invoke(a: Any, b: Any) = 
                op(b.asInstanceOf[T])
                kotlin.Unit.INSTANCE

        builder.list(elements, prefix, suffix, separator, builder.getMainSourcesetData, lambda)

    def addLink(
            text: String,
            address: DRI,
            kind: Kind = ContentKind.Main,
            sourceSets: JSet[DokkaConfiguration$DokkaSourceSet] = builder.getMainSourcesetData,
            styles: JSet[Style] = builder.getMainStyles,
            extra: PropertyContainer[ContentNode]= builder.getMainExtra) =
            builder.link(text, address, kind, sourceSets, styles, extra)

object JList:
    def apply[T](elem: T): JList[T] = List(elem).asJava
    def apply[T]() = List[T]().asJava

object JSet:
    def apply[T](elem: T): JSet[T] = Set(elem).asJava
    def apply[T]() = Set[T]().asJava
