package dotty.dokka

import org.jetbrains.dokka.base.signatures._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.base.signatures.KotlinSignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.utilities.DokkaLogger
import collection.JavaConverters._
import org.jetbrains.dokka.base.translators.documentables._
import org.jetbrains.dokka.model.properties.PropertyContainer
import dokka.java.api._
import java.util.function.Consumer
import kotlin.jvm.functions.Function2
import java.util.{List => JList}

class ScalaSignatureProvider(ctcc: CommentsToContentConverter, logger: DokkaLogger) extends SignatureProvider:
    private val default = new KotlinSignatureProvider(ctcc, logger)
    private val contentBuilder = new JPageContentBuilder(ctcc, this, logger)

    override def signature(documentable: Documentable) = documentable match {
        case method: DFunction =>
           List(methodSignature(method)).asJava
        case _ => default.signature(documentable)
    }

    val styles = Set(TextStyle.Monospace).asJava

    val utils: JvmSignatureUtils = KotlinSignatureUtils.INSTANCE

    private def methodSignature(method: DFunction): ContentNode = 
        val methodExtension = method.get(tasty.MethodExtension)
        content(method){ builder =>
            utils.annotationsBlock(builder, method)
            // builder.addText("TODO modifiers")
            builder.addText("def")
            builder.addText(" ")
            builder.addLink(method.getName, method.getDri)
            builder.addList(method.getGenerics, "[", "]")(e => builder.unaryPlus(builder.buildSignature(e)))
            val params = methodExtension.parametersListSizes.foldLeft(0){ (from, size) =>
                val toIndex = from + size
                builder.addList(method.getParameters.subList(from, toIndex), "(", ")"){ param =>
                    utils.annotationsInline(builder, param)
                    // builder.addText("TODO modifiers")
                    builder.addText(param.getName)
                    builder.addText(":")
                    builder.typeSignature(param.getType)
                }
                toIndex
            }
            builder.addText(":")
            builder.addText(" ")
            builder.typeSignature(method.getType)
        }
    
    extension on (builder: PageContentBuilder$DocumentableContentBuilder):
        def typeSignature(b: Projection): Unit = b match {
            case tc: TypeConstructor =>
                // TODO we should handle types differnetly...
                builder.addLink(tc.getDri.getClassNames, tc.getDri)
                builder.addList(tc.getProjections, "[", "]")(p => builder.typeSignature(p))

            case other =>
             builder.addText(s"TODO: $other")
        }

    private def content(d: Documentable)(render: PageContentBuilder$DocumentableContentBuilder => Unit): ContentGroup = 
        val lambda: Consumer[PageContentBuilder$DocumentableContentBuilder] = new Consumer:
          override def accept(v: PageContentBuilder$DocumentableContentBuilder) = render(v)

        contentBuilder.mkContent(d, ContentKind.Symbol, styles, lambda)