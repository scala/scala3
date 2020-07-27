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

    def (tokens: Seq[String]).toSignatureString(): String =
        tokens.filterNot(_.isBlank).mkString(""," "," ")

    override def signature(documentable: Documentable) = documentable match {
        case method: DFunction =>
           List(methodSignature(method)).asJava
        case clazz: DClass =>
           List(classSignature(clazz)).asJava
        case _ => default.signature(documentable)
    }

    val styles = Set(TextStyle.Monospace).asJava

    val utils: JvmSignatureUtils = KotlinSignatureUtils.INSTANCE

    private def classSignature(clazz: DClass): ContentNode = 
        content(clazz){ builder =>
            val ext = clazz.get(ClasslikeExtension)
            utils.annotationsBlock(builder, clazz)
            // builder.addText("TODO modifiers")
            builder.addText(
                Seq(
                    clazz.getVisibility.defaultValue.getName, 
                    clazz.getModifier.defaultValue.getName,
                    "class"
                ).toSignatureString()
            )
            builder.addLink(clazz.getName, clazz.getDri)
            builder.generics(clazz)
            ext.constructor.foreach(c => builder.functionParameters(c))
            ext.parentTypes match 
                case Nil =>
                case extendType :: withTypes =>
                    builder.addText(" extends ") 
                    builder.typeSignature(extendType)
                    withTypes.foreach { t => 
                        builder.addText(" with ")  
                        builder.typeSignature(extendType)
                    }
        }

    private def methodSignature(method: DFunction): ContentNode = 
        content(method){ builder =>
            utils.annotationsBlock(builder, method)
            // builder.addText("TODO modifiers")
            builder.addText(
                Seq(
                    method.getVisibility.defaultValue.getName, 
                    method.getModifier.defaultValue.getName,
                    "def"
                ).toSignatureString()
            )
            builder.addLink(method.getName, method.getDri)
            builder.generics(method)  
            builder.functionParameters(method)
            if !method.isConstructor then
                builder.addText(":")
                builder.addText(" ")
                builder.typeSignature(method.getType)
        }


    extension on (builder: PageContentBuilder$DocumentableContentBuilder):
        def typeSignature(b: Projection): Unit = b match {
            case tc: TypeConstructor =>
                tc.getProjections.asScala.foreach {
                    case text: UnresolvedBound => builder.addText(text.getName)
                    case link: OtherParameter => 
                        builder.addLink(link.getName, link.getDeclarationDRI)
                    case other =>
                        builder.addText(s"TODO($other)")
                }
            case other =>
             builder.addText(s"TODO: $other")
        }

        def generics(on: WithGenerics) = builder.addList(on.getGenerics, "[", "]"){ e => 
            builder.addText(e.getName)
            e.getBounds.forEach(b => builder.typeSignature(b))
        }
        
        def functionParameters(method: DFunction) = 
            val methodExtension = method.get(MethodExtension)
            methodExtension.parametersListSizes.foldLeft(0){ (from, size) =>
                val toIndex = from + size
                if from == toIndex then builder.addText("()")
                else builder.addList(method.getParameters.subList(from, toIndex), "(", ")"){ param =>
                    utils.annotationsInline(builder, param)
                    // builder.addText("TODO modifiers")
                    builder.addText(param.getName)
                    builder.addText(": ")
                    builder.typeSignature(param.getType)
                }
                toIndex
            }

    private def content(d: Documentable)(render: PageContentBuilder$DocumentableContentBuilder => Unit): ContentGroup = 
        val lambda: Consumer[PageContentBuilder$DocumentableContentBuilder] = new Consumer:
          override def accept(v: PageContentBuilder$DocumentableContentBuilder) = render(v)

        contentBuilder.mkContent(d, ContentKind.Symbol, styles, lambda)