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
import java.util.function.Consumer
import kotlin.jvm.functions.Function2
import org.jetbrains.dokka.links.DRI
import dotty.dokka.model.api.{Kind, _}


class ScalaSignatureProvider(contentConverter: CommentsToContentConverter, logger: DokkaLogger) extends SignatureProvider with ScalaSignatureUtils:
    private val default = new KotlinSignatureProvider(contentConverter, logger)
    private val styles = Set(TextStyle.Monospace).asInstanceOf[Set[Style]]
    private val contentBuilder = new ScalaPageContentBuilder(contentConverter, this, logger)

    private def signatureContent(d: Documentable)(
        func: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder
    ) = contentBuilder.contentForDocumentable(d, kind = ContentKind.Symbol, styles = styles, buildBlock = func)


    case class ContentNodeBuilder(builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder) extends SignatureBuilder{
        def text(str: String): SignatureBuilder = ContentNodeBuilder(builder.text(str))
        def driLink(text: String, dri: DRI): SignatureBuilder = ContentNodeBuilder(builder.driLink(text, dri))
    }

    override def signature(documentable: Documentable) =
        JList(signatureContent(documentable){ builder =>
            val withAnnotations = ContentNodeBuilder(builder).annotationsBlock(documentable)
            val res = ScalaSignatureProvider.rawSignature(documentable, withAnnotations)
            res.asInstanceOf[ContentNodeBuilder].builder
        })

object ScalaSignatureProvider:
    def rawSignature(documentable: Documentable, builder: SignatureBuilder): SignatureBuilder =
        documentable match
            case extension: DFunction if extension.kind.isInstanceOf[Kind.Extension] =>
                extensionSignature(extension, builder)
            case method: DFunction if method.kind.isInstanceOf[Kind.Given] =>
                givenMethodSignature(method, builder)
            case method: DFunction =>
                methodSignature(method, builder)
            case enumEntry: DClass if enumEntry.kind == Kind.EnumCase =>
                enumEntrySignature(enumEntry, builder)
            case clazz: DClass =>
                classSignature(clazz, builder)
            case enumProperty: DProperty if enumProperty.kind == Kind.EnumCase =>
                enumPropertySignature(enumProperty, builder)
            case property: DProperty =>
                propertySignature(property, builder)
            case parameter: DParameter =>
                parameterSignature(parameter, builder)
            case _ =>
                ???


    private def enumEntrySignature(entry: DClass, bdr: SignatureBuilder): SignatureBuilder =
        val ext = entry.get(ClasslikeExtension)
        val withPrefixes: SignatureBuilder = bdr
            .text("case ")
            .name(entry.getName, entry.getDri)
            .generics(entry)

        val withParameters = ext.constructor.toSeq.foldLeft(withPrefixes){ (bdr, elem) =>
            bdr.functionParameters(elem)
        }
        parentsSignature(entry, withParameters)

    private def enumPropertySignature(entry: DProperty, builder: SignatureBuilder): SignatureBuilder =
        val modifiedType = entry.getType match
            case t: TypeConstructor => GenericTypeConstructor(
                t.getDri,
                t.getProjections.asScala.map{
                    case t: UnresolvedBound if t.getName == " & " => UnresolvedBound(" with ");
                    case other => other
                }.asJava,
                null
            )
            case other => other

        builder
            .text("case ")
            .name(entry.getName, entry.getDri)
            .text(" extends ")
            .typeSignature(modifiedType)

    private def parentsSignature(d: DClass, builder: SignatureBuilder): SignatureBuilder =
        d.directParents match
            case Nil => builder
            case extendType :: withTypes =>
                val extendPart = builder.text(" extends ").signature(extendType)
                withTypes.foldLeft(extendPart)((bdr, tpe) => bdr.text(" with ").signature(tpe))

    private def classSignature(clazz: DClass, builder: SignatureBuilder): SignatureBuilder =
        val ext = clazz.get(ClasslikeExtension)
        val prefixes = builder
            .modifiersAndVisibility(clazz, clazz.kind.name)
            .name(clazz.getName, clazz.getDri)
            .generics(clazz)

        val withGenerics = ext.constructor.toSeq.foldLeft(prefixes){ (bdr, elem) =>
            bdr.functionParameters(elem)
        }
        parentsSignature(clazz, withGenerics)

    private def extensionSignature(extension: DFunction, builder: SignatureBuilder): SignatureBuilder =
        val extendedSymbol = if (extension.isRightAssociative()) {
            extension.getParameters.asScala(extension.get(MethodExtension).parametersListSizes(0))
        } else {
            extension.getParameters.asScala(0)
        }
        val withSinature = builder
            .modifiersAndVisibility(extension, "def")
            .name(extension.getName, extension.getDri)
            .generics(extension)
            .functionParameters(extension)

        if extension.isConstructor then withSinature
        else withSinature.text(":").text(" ").typeSignature(extension.getType)

    private def givenMethodSignature(method: DFunction, builder: SignatureBuilder): SignatureBuilder = method.kind match
        case Kind.Given(Some(instance), _) =>
            builder.text("given ")
                .name(method.getName, method.getDri)
                .text(" as ")
                .signature(instance)
        case _ =>
            builder.text("given ").name(method.getName, method.getDri)


    private def methodSignature(method: DFunction, builder: SignatureBuilder): SignatureBuilder =
        val bdr = builder
        .modifiersAndVisibility(method, "def")
        .name(method.getName, method.getDri)
        .generics(method)
        .functionParameters(method)
        if !method.isConstructor then
            bdr
                .text(":")
                .text(" ")
                .typeSignature(method.getType)
        else bdr


    private def propertySignature(property: DProperty, builder: SignatureBuilder): SignatureBuilder =
        property.kind match
            case _: Kind.Given => givenPropertySignature(property, builder)
            case tpe: Kind.Type => typeSignature(tpe, property, builder)
            case other => fieldSignature(property, other.name, builder)


    private def typeSignature(tpe: Kind.Type, typeDef: DProperty, builder: SignatureBuilder): SignatureBuilder =
        val bdr = builder
            .modifiersAndVisibility(typeDef, if tpe.opaque then "opaque type" else "type")
            .name(typeDef.getName, typeDef.getDri)
            .generics(typeDef)
        if(!tpe.opaque){
            (if tpe.concreate then bdr.text(" = ") else bdr)
                .typeSignature(typeDef.getType)
        } else bdr


    private def givenPropertySignature(property: DProperty, builder: SignatureBuilder): SignatureBuilder =
        val bdr = builder
            .text("given ")
            .name(property.getName, property.getDri)

        property.kind match
            case Kind.Given(Some(instance), _) =>
                 bdr.text(" as ").signature(instance)
            case _ => bdr

    private def fieldSignature(property: DProperty, kind: String, builder: SignatureBuilder): SignatureBuilder =
        builder
            .modifiersAndVisibility(property, kind)
            .name(property.getName, property.getDri)
            .text(":")
            .text(" ")
            .typeSignature(property.getType)

    private def parameterSignature(parameter: DParameter, builder: SignatureBuilder): SignatureBuilder =
        val ext = parameter.get(ParameterExtension)
        builder
            .text(if ext.isGrouped then "extension (" else "(")
            .text(parameter.getName)
            .text(": ")
            .typeSignature(parameter.getType)
            .text(")")
