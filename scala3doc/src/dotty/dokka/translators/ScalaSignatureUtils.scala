package dotty.dokka

import org.jetbrains.dokka.base.signatures._
import org.jetbrains.dokka.base.translators.documentables.PageContentBuilder
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.model.properties.WithExtraProperties
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import dotty.dokka.model.api.{Kind, _}

case class InlineSignatureBuilder(names: Signature = Nil, preName: Signature = Nil) extends SignatureBuilder:
    override def text(str: String): SignatureBuilder = copy(names = str +: names)
    override def name(str: String, dri: DRI): SignatureBuilder = copy(names = Nil, preName = names)
    override def driLink(text: String, dri: DRI): SignatureBuilder = copy(names = Link(text, dri) +: names)

object InlineSignatureBuilder:
    def typeSignatureFor(d: Documentable): Signature =
            ScalaSignatureProvider.rawSignature(d, InlineSignatureBuilder()).asInstanceOf[InlineSignatureBuilder].names.reverse

trait SignatureBuilder extends ScalaSignatureUtils {
    def text(str: String): SignatureBuilder
    def name(str: String, dri: DRI) = driLink(str, dri)
    def driLink(text: String, dri: DRI): SignatureBuilder

    def signature(s: Signature) = s.foldLeft(this){ (b, e) => e match
        case Link(name, dri) => b.driLink(name, dri)
        case txt: String => b.text(txt)
    }

    def list[E](
            elements: List[E],
            prefix: String = "",
            suffix: String = "",
            separator: String = ", ",
        )(
            elemOp: (SignatureBuilder, E) => SignatureBuilder
        ): SignatureBuilder = elements match {
            case Nil => this
            case head :: tail =>
                tail.foldLeft(elemOp(text(prefix), head))((b, e) => elemOp(b.text(separator), e)).text(suffix)
        }

    def annotationsBlock(d: Member): SignatureBuilder =
            d.annotations.foldLeft(this){ (bdr, annotation) => bdr.buildAnnotation(annotation)}

        def annotationsInline(d: Documentable with WithExtraProperties[_]): SignatureBuilder =
                d.annotations.foldLeft(this){ (bdr, annotation) => bdr.buildAnnotation(annotation) }

        private def buildAnnotation(a: Annotation): SignatureBuilder =
           text("@").driLink(a.dri.getClassNames, a.dri).buildAnnotationParams(a).text(" ")

        private def buildAnnotationParams(a: Annotation): SignatureBuilder =
            if !a.params.isEmpty then
                list(a.params, "(", ")", ", "){ (bdr, param) => bdr.buildAnnotationParameter(param)}
            else this

        private def addParameterName(txt: Option[String]): SignatureBuilder = txt match {
                case Some(name) => this.text(s"$txt = ")
                case _ => this
            }

        private def buildAnnotationParameter(a: Annotation.AnnotationParameter): SignatureBuilder = a match {
            case Annotation.PrimitiveParameter(name, value) =>
                addParameterName(name).text(value)
            case Annotation.LinkParameter(name, dri, text) =>
                addParameterName(name).driLink(text, dri)
            case Annotation.UnresolvedParameter(name, value) =>
                addParameterName(name).text(value)
        }

        def modifiersAndVisibility(t: Documentable with WithAbstraction with WithVisibility with WithExtraProperties[_], kind: String) =
            import org.jetbrains.dokka.model.properties._
            val extras = t.getExtra.getMap()
            val (prefixMods, suffixMods) = t.modifiers.partition(_.prefix)
            val all = prefixMods.map(_.name) ++ Seq(t.visibility.asSignature) ++ suffixMods.map(_.name)

            text(all.toSignatureString()).text(kind + " ")

        def typeSignature(b: Projection): SignatureBuilder = b match {
            case tc: TypeConstructor =>
                tc.getProjections.asScala.foldLeft(this) { (bdr, elem) => elem match {
                    case text: UnresolvedBound => bdr.text(text.getName)
                    case link: TypeParameter =>
                        bdr.driLink(link.getName, link.getDri)
                    case other =>
                        bdr.text(s"TODO($other)")
                }
            }
            case other =>
                text(s"TODO: $other")
        }

        def generics(on: WithGenerics) = list(on.getGenerics.asScala.toList, "[", "]"){ (bdr, e) =>
            val bldr = bdr.text(e.getName)
            e.getBounds.asScala.foldLeft(bldr)( (b, bound) => b.typeSignature(bound))
        }

        def functionParameters(method: DFunction) =
            val methodExtension = method.get(MethodExtension)
            val receiverPos = if method.isRightAssociative() then methodExtension.parametersListSizes(0) else 0
            val (bldr, index) = methodExtension.parametersListSizes.foldLeft(this, 0){
                case ((builder, from), size) =>
                    val toIndex = from + size
                    if from == toIndex then (builder.text("()"), toIndex)
                    else if !method.kind.isInstanceOf[Kind.Extension] || from != receiverPos then
                        val b = builder.list(method.getParameters.subList(from, toIndex).asScala.toList, "(", ")"){ (bdr, param) => bdr
                            .annotationsInline(param)
                            .text(param.getName)
                            .text(": ")
                            .typeSignature(param.getType)
                        }
                        (b, toIndex)
                    else (builder, toIndex)
            }
            bldr
}

trait ScalaSignatureUtils:
    extension (tokens: Seq[String]) def toSignatureString(): String =
        tokens.filter(_.trim.nonEmpty).mkString(""," "," ")
