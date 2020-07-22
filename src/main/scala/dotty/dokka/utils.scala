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
import java.util.{List => JList, Set => JSet}
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet

extension  on[T, V] (a: WithExtraProperties[T]):
  def get(key: ExtraProperty.Key[_ >: T, V]): V = a.getExtra().getMap().get(key).asInstanceOf[V]


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
