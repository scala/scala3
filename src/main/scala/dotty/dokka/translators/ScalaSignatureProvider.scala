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

class ScalaSignatureProvider(contentConverter: CommentsToContentConverter, logger: DokkaLogger) extends SignatureProvider:
    private val default = new KotlinSignatureProvider(contentConverter, logger)
    private val contentBuilder = new PageContentBuilder(contentConverter, this, logger)

    def (tokens: Seq[String]).toSignatureString(): String =
        tokens.filter(_.trim.nonEmpty).mkString(""," "," ")

    override def signature(documentable: Documentable) = documentable match {
        case extension: DFunction if extension.get(MethodExtension).extensionInfo.isDefined =>
            JList(extensionSignature(extension))
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

    val styles = JSet(TextStyle.Monospace)

    val utils: JvmSignatureUtils = KotlinSignatureUtils.INSTANCE

    private def enumEntrySignature(entry: DClass): ContentNode =
        content(entry){ builder =>
            val ext = entry.get(ClasslikeExtension)
            builder.addText("case ")
            builder.addLink(entry.getName, entry.getDri)
            builder.generics(entry)
            ext.constructor.foreach(c => builder.functionParameters(c))
            ext.parentTypes match 
                case Nil =>
                case extendType :: withTypes =>
                    builder.addText(" extends ") 
                    builder.typeSignature(extendType)
                    withTypes.foreach { withType => 
                        builder.addText(" with ")  
                        builder.typeSignature(withType)
                    }
            }

    private def enumPropertySignature(entry: DProperty): ContentNode = 
        content(entry){ builder =>
            builder.addText("case ")
            builder.addLink(entry.getName, entry.getDri)
            builder.addText(" extends ")
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
            builder.typeSignature(modifiedType)

        }

    private def classSignature(clazz: DClass): ContentNode = 
        content(clazz){ builder =>
            val ext = clazz.get(ClasslikeExtension)
            utils.annotationsBlock(builder, clazz)
            builder.modifiersAndVisibility(clazz, ext.kind.name)
            builder.addLink(clazz.getName, clazz.getDri)
            builder.generics(clazz)
            ext.constructor.foreach(c => builder.functionParameters(c))
            ext.parentTypes match 
                case Nil =>
                case extendType :: withTypes =>
                    builder.addText(" extends ") 
                    builder.typeSignature(extendType)
                    withTypes.foreach { withType => 
                        builder.addText(" with ")  
                        builder.typeSignature(withType)
                    }
        }

    private def extensionSignature(extension: DFunction): ContentNode =
        content(extension){ builder =>
            val grouped = extension.get(MethodExtension).extensionInfo.map(_.isGrouped).getOrElse(false)
            val extendedSymbol = if (extension.isRightAssociative()) {
                extension.getParameters.asScala(extension.get(MethodExtension).parametersListSizes(0))
            } else {
                extension.getParameters.asScala(0)
            }

            utils.annotationsBlock(builder, extension)
            if(!grouped){
                builder.addText("extension ")
                builder.addText(s" (${extendedSymbol.getName}: ")
                builder.typeSignature(extendedSymbol.getType)
                builder.addText(") ")
            }
            builder.modifiersAndVisibility(extension, "def")
            builder.addLink(extension.getName, extension.getDri)
            builder.generics(extension)  
            builder.functionParameters(extension)
            if !extension.isConstructor then
                builder.addText(":")
                builder.addText(" ")
                builder.typeSignature(extension.getType)
        }

    private def methodSignature(method: DFunction): ContentNode = 
        content(method){ builder =>
            val isGiven = method.get(IsGiven)
            utils.annotationsBlock(builder, method)
            if(isGiven != null) {
                builder.addText("given ")
                if(isGiven.givenInstance.isDefined){
                    builder.addLink(method.getName, method.getDri)
                } else {
                    builder.typeSignature(method.getType)
                }
            } else {
                builder.modifiersAndVisibility(method, "def")
                builder.addLink(method.getName, method.getDri)
            }
            
            builder.generics(method)  
            builder.functionParameters(method)
            if !method.isConstructor && isGiven == null then
                builder.addText(":")
                builder.addText(" ")
                builder.typeSignature(method.getType)
            else if isGiven != null && isGiven.givenInstance.isDefined then
                builder.addText(" as ")
                isGiven.givenInstance.foreach(t => builder.typeSignature(t))
        }

    private def propertySignature(property: DProperty): ContentNode = 
        property.get(PropertyExtension).kind match
            case "type" => typeSignature(property)
            case other => fieldSignature(property, other)

    private def typeSignature(typeDef: DProperty): ContentNode =
        content(typeDef){ builder =>
            val modifiers = typeDef.getExtra.getMap.get(AdditionalModifiers.Companion).asInstanceOf[AdditionalModifiers]
            val isOpaque = modifiers != null && modifiers.getContent.defaultValue.asScala.contains(ScalaOnlyModifiers.Opaque)
            utils.annotationsBlock(builder, typeDef)
            // builder.addText("TODO modifiers")
            builder.modifiersAndVisibility(typeDef, "type")
            builder.addLink(typeDef.getName, typeDef.getDri)
            builder.generics(typeDef)
            if(!isOpaque){
                if !typeDef.get(PropertyExtension).isAbstract then builder.addText(" = ")
                builder.typeSignature(typeDef.getType)
            } 
        } 

    private def fieldSignature(property: DProperty, kind: String): ContentNode =
        content(property){ builder =>
            utils.annotationsBlock(builder, property)
            val isGiven = property.get(IsGiven)
            if(isGiven != null) {
                builder.addText("given ")
            }
            else {
                builder.modifiersAndVisibility(property, kind)
            }
            // builder.addText("TODO modifiers")
            builder.addLink(property.getName, property.getDri)
            if(isGiven != null && isGiven.givenInstance.isDefined) {
                builder.addText(" as ")
                isGiven.givenInstance.foreach(t => builder.typeSignature(t))
            } else if (isGiven == null) {
                builder.addText(":")
                builder.addText(" ")
                builder.typeSignature(property.getType)
            }
        }    

    private def parameterSignature(parameter: DParameter): ContentNode = 
        val ext = parameter.get(ParameterExtension)
        content(parameter){ builder =>
            if(ext.isGrouped){
                builder.addText("extension (")
            } else {
                builder.addText("(")
            }
            builder.addText(parameter.getName)
            builder.addText(": ")
            builder.typeSignature(parameter.getType)
            builder.addText(")")
        }

    extension on(f: DFunction):
        def isRightAssociative(): Boolean = f.getName.endsWith(":")


    extension on (builder: PageContentBuilder$DocumentableContentBuilder):

        def modifiersAndVisibility[T <: Documentable](t: WithAbstraction with WithVisibility with WithExtraProperties[T], kind: String) =
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


            builder.addText(
                Seq(
                    prefixes.trim,
                    t.getVisibility.defaultValue.getName, 
                    t.getModifier.defaultValue.getName,
                    suffixes.trim,
                    kind
                ).toSignatureString()
            )

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
            val receiverPos = if method.isRightAssociative() then method.get(MethodExtension).parametersListSizes(0) else 0
            val paramLists = methodExtension.parametersListSizes
            paramLists.foldLeft(0){ (from, size) =>
                val toIndex = from + size
                if from == toIndex then builder.addText("()")
                else if !methodExtension.extensionInfo.isDefined || from != receiverPos then
                    builder.addList(method.getParameters.subList(from, toIndex), "(", ")"){ param =>
                    utils.annotationsInline(builder, param)
                    // builder.addText("TODO modifiers")
                    builder.addText(param.getName)
                    builder.addText(": ")
                    builder.typeSignature(param.getType)
                }
                toIndex
            }

    private def content(d: Documentable)(render: PageContentBuilder$DocumentableContentBuilder => Unit): ContentGroup = 
        val lambda : kotlin.jvm.functions.Function1[? >: PageContentBuilder#DocumentableContentBuilder, kotlin.Unit] = 
            r => 
                render(r.asInstanceOf[PageContentBuilder$DocumentableContentBuilder])
                kotlin.Unit.INSTANCE

        contentBuilder.contentFor(d.getDri, d.getSourceSets, ContentKind.Symbol, styles, PropertyContainer.Companion.empty(), lambda)
