package dotty.dokka

import org.jetbrains.dokka.base.signatures._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.model.properties.{WithExtraProperties}
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

class ScalaSignatureProvider(contentConverter: CommentsToContentConverter, logger: DokkaLogger) extends SignatureProvider with ScalaSignatureUtils:
    private val default = new KotlinSignatureProvider(contentConverter, logger)
    private val styles = Set(TextStyle.Monospace).asInstanceOf[Set[Style]]
    private val contentBuilder = new ScalaPageContentBuilder(contentConverter, this, logger)

    private def signatureContent(d: Documentable)(
        func: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ) = contentBuilder.contentForDocumentable(d, kind = ContentKind.Symbol, styles = styles, buildBlock = func)


    override def signature(documentable: Documentable) = documentable match {
        case extension: DFunction if extension.get(MethodExtension).extensionInfo.isDefined =>
            JList(extensionSignature(extension))
        case method: DFunction if method.get(IsGiven) != null =>
            JList(givenMethodSignature(method))
        case method: DFunction =>
            JList(methodSignature(method))
        case enumEntry: DClass if enumEntry.get(IsEnumEntry) != null => 
            JList(enumEntrySignature(enumEntry))
        case clazz: DClass =>
            JList(classSignature(clazz))
        case enumProperty: DProperty if enumProperty.get(IsEnumEntry) != null => 
            JList(enumPropertySignature(enumProperty))
        case property: DProperty =>
            JList(propertySignature(property))
        case parameter: DParameter =>
            JList(parameterSignature(parameter))
        case _ => default.signature(documentable)
    }


    private def enumEntrySignature(entry: DClass): ContentNode =
        val ext = entry.get(ClasslikeExtension)
        signatureContent(entry){ bdr => 
            val temp = bdr
                .text("case ")
                .driLink(entry.getName, entry.getDri)
                .generics(entry)

            val temp2 = ext.constructor.toSeq.foldLeft(temp){ (bdr, elem) =>
                bdr.functionParameters(elem)
            }
            ext.parentTypes match{
                case Nil => temp2
                case extendType :: withTypes =>
                    val temp3 = temp2
                        .text(" extends ")
                        .typeSignature(extendType)
                    withTypes.foldLeft(temp3){ (bdr, tpe) =>
                        bdr.text(" with ").typeSignature(tpe)

                    }
            }
        }

    private def enumPropertySignature(entry: DProperty): ContentNode = 
        val modifiedType = entry.getType match{
            case t: TypeConstructor => TypeConstructor(
                t.getDri,
                t.getProjections.asScala.map{ 
                    case t: UnresolvedBound if t.getName == " & " => UnresolvedBound(" with "); 
                    case other => other
                }.asJava,
                t.getModifier
            )
            case other => other
        }
        signatureContent(entry){ builder => builder
            .text("case ")
            .driLink(entry.getName, entry.getDri)
            .text(" extends ")
            .typeSignature(modifiedType)

        }

    private def classSignature(clazz: DClass): ContentNode = 
        val ext = clazz.get(ClasslikeExtension)
        signatureContent(clazz){ builder =>
            val temp = builder
                .annotationsBlock(clazz)
                .modifiersAndVisibility(clazz, ext.kind.name)
                .driLink(clazz.getName, clazz.getDri)
                .generics(clazz)
            val temp2 = ext.constructor.toSeq.foldLeft(temp){ (bdr, elem) =>
                bdr.functionParameters(elem)
            }
            ext.parentTypes match{
                case Nil => temp2
                case extendType :: withTypes =>
                    val temp3 = temp2
                        .text(" extends ")
                        .typeSignature(extendType)
                    withTypes.foldLeft(temp3){ (bdr, tpe) =>
                        bdr.text(" with ").typeSignature(tpe)
                    }
            }
        }

    private def extensionSignature(extension: DFunction): ContentNode =
        signatureContent(extension){ builder =>
            val grouped = extension.get(MethodExtension).extensionInfo.map(_.isGrouped).getOrElse(false)
            val extendedSymbol = if (extension.isRightAssociative()) {
                extension.getParameters.asScala(extension.get(MethodExtension).parametersListSizes(0))
            } else {
                extension.getParameters.asScala(0)
            }
            val bldr = builder.annotationsBlock(extension)
            val bdr = (
                if(!grouped){
                bldr
                    .text("extension ")
                    .text(s" (${extendedSymbol.getName}: ")
                    .typeSignature(extendedSymbol.getType)
                    .text(") ")
                } else bldr
            )
                .modifiersAndVisibility(extension, "def")
                .driLink(extension.getName, extension.getDri)
                .generics(extension)  
                .functionParameters(extension)
            if !extension.isConstructor then
                bdr
                    .text(":")
                    .text(" ")
                    .typeSignature(extension.getType)
            else bdr
        }

    private def givenMethodSignature(method: DFunction): ContentNode = signatureContent(method){
        builder => 
            val bdr = builder
                .text("given ")
            method.get(IsGiven).givenInstance match {
                case Some(instance) => bdr
                    .driLink(method.getName, method.getDri)
                    .text(" as ")
                    .typeSignature(instance)
                case None => bdr.typeSignature(method.getType)
            }
        }

    private def methodSignature(method: DFunction): ContentNode = signatureContent(method){ 
        builder => 
            val bdr = builder
            .annotationsBlock(method)
            .modifiersAndVisibility(method, "def")
            .driLink(method.getName, method.getDri)
            .generics(method)  
            .functionParameters(method)
            if !method.isConstructor then
                bdr
                    .text(":")
                    .text(" ")
                    .typeSignature(method.getType)
            else bdr
        }

    private def propertySignature(property: DProperty): ContentNode = 
        property.get(PropertyExtension).kind match
            case kind if property.get(IsGiven) != null => givenPropertySignature(property)
            case "type" => typeSignature(property)
            case other => fieldSignature(property, other)

    private def typeSignature(typeDef: DProperty): ContentNode =
        val modifiers = typeDef.getExtra.getMap.get(AdditionalModifiers.Companion).asInstanceOf[AdditionalModifiers]
        val isOpaque = modifiers != null && modifiers.getContent.defaultValue.asScala.contains(ScalaOnlyModifiers.Opaque)
        signatureContent(typeDef){ builder => 
            val bdr = builder
                .annotationsBlock(typeDef)
                .modifiersAndVisibility(typeDef, "type")
                .driLink(typeDef.getName, typeDef.getDri)
                .generics(typeDef)
            if(!isOpaque){
                (if !typeDef.get(PropertyExtension).isAbstract then bdr.text(" = ") else bdr)
                    .typeSignature(typeDef.getType)
            } else bdr
        } 

    private def givenPropertySignature(property: DProperty): ContentNode = signatureContent(property){ 
        builder =>
            val bdr = builder
                .text("given ")
                .driLink(property.getName, property.getDri)
            property.get(IsGiven).givenInstance match {
                case Some(instance) => bdr
                    .text(" as ")
                    .typeSignature(instance)
                case None => bdr
            }
    }
        

    private def fieldSignature(property: DProperty, kind: String): ContentNode =
        signatureContent(property){ builder => builder
            .annotationsBlock(property)
            .modifiersAndVisibility(property, kind)
            .driLink(property.getName, property.getDri)
            .text(":")
            .text(" ")
            .typeSignature(property.getType)
        }    

    private def parameterSignature(parameter: DParameter): ContentNode = 
        val ext = parameter.get(ParameterExtension)
        signatureContent(parameter){ builder => builder
            .text(if ext.isGrouped then "extension (" else "(")
            .text(parameter.getName)
            .text(": ")
            .typeSignature(parameter.getType)
            .text(")")
        }
