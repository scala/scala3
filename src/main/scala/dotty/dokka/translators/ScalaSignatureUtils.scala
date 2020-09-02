package dotty.dokka

import org.jetbrains.dokka.base.signatures._
import org.jetbrains.dokka.base.translators.documentables.PageContentBuilder
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.model.properties.WithExtraProperties
import org.jetbrains.dokka.pages._
import collection.JavaConverters._

trait ScalaSignatureUtils:
    private val ignoreRules: List[(AnnotationsInfo.Annotation) => Boolean] = List(
        a => a.dri.getPackageName.startsWith("scala.annotation.internal")
    )

    extension (tokens: Seq[String]) def toSignatureString(): String =
        tokens.filter(_.trim.nonEmpty).mkString(""," "," ")

    extension [T <: Documentable] (d: T) def annotations() = (d match {
        case e: WithExtraProperties[T] => e.get(AnnotationsInfo).annotations
        case _ => List.empty
    }).filterNot(annotation => ignoreRules.exists(ignoreFun => ignoreFun(annotation)))

    extension [T <: Documentable](builder: ScalaPageContentBuilder#ScalaDocumentableContentBuilder):
        def annotationsBlock(d: Documentable): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = builder
            .group(styles = Set(TextStyle.Block), kind = ContentKind.Annotations){ bdr => 
                d.annotations().foldLeft(bdr){ (bdr, annotation) => bdr
                    .buildAnnotation(annotation)
                }
            }
        
        def annotationsInline(d: Documentable): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = builder
            .group(styles = Set(TextStyle.Span), kind = ContentKind.Annotations){ bdr => 
                d.annotations().foldLeft(bdr){ (bdr, annotation) => bdr
                    .buildAnnotation(annotation)
                }
            }

        private def buildAnnotation(a: AnnotationsInfo.Annotation): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = builder
            .group(){ bdr => bdr
                .text("@")
                .driLink(a.dri.getClassNames, a.dri)
                .buildAnnotationParams(a)
                .text(" ")
            }


        private def buildAnnotationParams(a: AnnotationsInfo.Annotation): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = 
            if !a.params.isEmpty then builder
                .group(styles = Set(TextStyle.BreakableAfter)){ bdr => bdr
                    .list(a.params, "(", ")", ", "){ (bdr, param) => bdr
                        .buildAnnotationParameter(param)
                    }
                }
            else builder

        private def addParameterName(txt: Option[String]): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = txt match {
                case Some(name) => builder.text(s"$txt = ")
                case _ => builder
            }

        private def buildAnnotationParameter(a: AnnotationsInfo.AnnotationParameter): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = a match {
            case AnnotationsInfo.PrimitiveParameter(name, value) => builder
                .addParameterName(name)
                .text(value)
            case AnnotationsInfo.LinkParameter(name, dri, text) => builder
                .addParameterName(name)
                .driLink(text, dri)
            case AnnotationsInfo.UnresolvedParameter(name, value) => builder
                .addParameterName(name)
                .text(value)
        }

        def modifiersAndVisibility(t: WithAbstraction with WithVisibility with WithExtraProperties[T], kind: String) =
            import org.jetbrains.dokka.model.properties._
            val extras = t.getExtra.getMap()
            val additionalModifiers =
              Option(extras.get(AdditionalModifiers.Companion).asInstanceOf[AdditionalModifiers])
                .map(_.getContent)
                .map(content => content.defaultValue.asScala.collect{case s: ScalaOnlyModifiers => s})
                
            val prefixes = additionalModifiers
                .map(_.filter(_.prefix).map(_.getName))
                .map(modifiers => modifiers.toSeq.toSignatureString())
                .getOrElse("")
            
            val suffixes = additionalModifiers
                .map(_.filter(!_.prefix).map(_.getName))
                .map(modifiers => modifiers.toSeq.toSignatureString())
                .getOrElse("")


            builder
                .text(
                    Seq(
                        prefixes.trim,
                        t.getVisibility.defaultValue.getName, 
                        t.getModifier.defaultValue.getName,
                        suffixes.trim,
                        kind
                    ).toSignatureString()
                )

        def typeSignature(b: Projection): ScalaPageContentBuilder#ScalaDocumentableContentBuilder = b match {
            case tc: TypeConstructor =>
                tc.getProjections.asScala.foldLeft(builder) { (bdr, elem) => elem match {
                    case text: UnresolvedBound => bdr.text(text.getName)
                    case link: OtherParameter => 
                        bdr.driLink(link.getName, link.getDeclarationDRI)
                    case other =>
                        bdr.text(s"TODO($other)")
                }
            }
            case other =>
                builder.text(s"TODO: $other")
        }

        def generics(on: WithGenerics) = builder.list(on.getGenerics.asScala.toList, "[", "]"){ (bdr, e) => 
            val bldr = bdr.text(e.getName)
            e.getBounds.asScala.foldLeft(bldr)( (b, bound) => b.typeSignature(bound))
        }
        
        def functionParameters(method: DFunction) = 
            val methodExtension = method.get(MethodExtension)
            val receiverPos = if method.isRightAssociative() then method.get(MethodExtension).parametersListSizes(0) else 0
            val paramLists = methodExtension.parametersListSizes
            val (bldr, index) = paramLists.foldLeft(builder, 0){
                case ((builder, from), size) =>
                    val toIndex = from + size
                    if from == toIndex then (builder.text("()"), toIndex)
                    else if !methodExtension.extensionInfo.isDefined || from != receiverPos then
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


    
    