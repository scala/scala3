package dotty.dokka

import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
import org.jetbrains.dokka.model._
import collection.JavaConverters
import collection.JavaConverters._
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.properties._

class ImplicitMembersExtensionTransformer(ctx: DokkaContext) extends DocumentableTransformer:
    override def invoke(original: DModule, context: DokkaContext): DModule = propagateImplicitConversionsAndExtensionMethods(original, classlikeMap(original))


    private def classlikeMap(m: DModule): Map[DRI, DClass] = {
        def getEntries(d: DClass): List[(DRI, DClass)] = d.getClasslikes.asScala
            .collect{ case d: DClass => d }.toList
            .flatMap(getEntries(_)) :+ (d.getDri, d)

        m.getPackages.asScala
            .flatMap(_.getClasslikes.asScala)
            .collect{ case d: DClass => d }
            .flatMap(getEntries(_))
            .toMap
    }

    private def propagateImplicitConversionsAndExtensionMethods[T <: Documentable](
        d: T, 
        classlikeMap: Map[DRI, DClass],
        currentConversions: List[ImplicitConversion] = List.empty,
        currentExtensions: List[(Documentable, ExtensionGroup)] = List.empty,
    ): T = d match {
        case m: DModule => m.copy(
            m.getName,
            m.getPackages.asScala.map(m => propagateImplicitConversionsAndExtensionMethods(m, classlikeMap)).asJava,
            m.getDocumentation,
            m.getExpectPresentInSet,
            m.getSourceSets,
            m.getExtra
        ).asInstanceOf[T]
        case p: DPackage => p.copy(
            p.getDri,
            p.getFunctions,
            p.getProperties,
            p.getClasslikes.asScala.map(c => 
                propagateImplicitConversionsAndExtensionMethods(c, classlikeMap)
            ).asJava,
            p.getTypealiases,
            p.getDocumentation,
            p.getExpectPresentInSet,
            p.getSourceSets,
            p.getExtra
        ).asInstanceOf[T]
        case c: DClass => 
            def modifyExtensionFunction(e: DFunction) = {
                val oldInfo = e.get(MethodExtension)
                if(oldInfo.extensionInfo.map(_.isGrouped).get) e
                else e.withNewExtras(
                    PropertyContainer.Companion.empty.addAll(
                        (e.getExtra.getMap.asScala.map(_._2).toList.filterNot(_.isInstanceOf[MethodExtension])
                            :+ MethodExtension(oldInfo.parametersListSizes, Some(ExtensionInformation(true)))).asJava
                    )
                )
            }
            val companion = Option(c.get(ClasslikeExtension))
                .flatMap(_.companion)
                .map(classlikeMap(_))
            val companionObjectConversions = companion
                .map(_.get(ImplicitConversions))
                .filter(_ != null)
                .fold(List.empty)(_.conversions)

            val companionObjectExtensions = companion
                .map(c => c -> c.get(ClasslikeExtension))
                .filter(_(1) != null)
                .map(
                    (companion, ext) => companion -> ext.extensions
                )
                .fold(List.empty)(
                    (companion, extensions) => extensions.map(companion -> _)
                )


            val implicits = (currentConversions ++ companionObjectConversions)
                .filter(_.from == c.getDri)
                .map(conv => conv.conversion -> classlikeMap(conv.to))
            val extensionMethods = (currentExtensions ++ companionObjectExtensions)
                .filter(_._2._1.getType.asInstanceOf[org.jetbrains.dokka.model.TypeConstructor].getDri == c.getDri)
                .map(e => e._1 -> e._2.extensions)
                .flatMap(e => e._2.map(_ -> e._1))
                .map( (key,value) => modifyExtensionFunction(key) -> value )
                .toMap
            val implicitMembers = ImplicitMembers(
                implicits.flatMap( (conv, i) => i.getFunctions.asScala.toList.map(_ -> conv)).toMap,
                implicits.flatMap( (conv, i) => i.get(ClasslikeExtension).inherited.methods.map(_ -> conv)).toMap,
                implicits.flatMap( (conv, i) => i.getProperties.asScala.filter(_.get(PropertyExtension).kind != "type" ).toList.map(_ -> conv)).toMap,
                implicits.flatMap( (conv, i) => i.get(ClasslikeExtension).inherited.fields.map(_ -> conv)).toMap,
                extensionMethods
            )
            c.copy(
                c.getDri,
                c.getName,
                c.getConstructors,
                c.getFunctions,
                c.getProperties,
                c.getClasslikes.asScala.map(cl => 
                    propagateImplicitConversionsAndExtensionMethods(
                        cl, 
                        classlikeMap,
                        currentConversions ++ c.get(ImplicitConversions).conversions,
                        currentExtensions ++ c.get(ClasslikeExtension).extensions.map(c -> _)
                    )
                ).asJava,
                c.getSources,
                c.getVisibility,
                c.getCompanion,
                c.getGenerics,
                c.getSupertypes,
                c.getDocumentation,
                c.getExpectPresentInSet,
                c.getModifier,
                c.getSourceSets,
                c.getExtra
            ).withNewExtras(
                c.getExtra.plus(
                    implicitMembers
                )
            ).asInstanceOf[T]
        case other => other
    }

