package dotty.dokka

import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
import org.jetbrains.dokka.model._
import collection.JavaConverters
import collection.JavaConverters._
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.properties._

class InheritanceInformationTransformer(val ctx: DokkaContext) extends DocumentableTransformer{
    override def invoke(original: DModule, context: DokkaContext): DModule = {
        val supertypes = getSupertypes(original)
        val subtypes = getSubtypesMap(supertypes)
        completeInheritanceInformation(original, subtypes)
    }

    private def getSupertypes(d: Documentable): List[(DRIWithKind, DRIWithKind)] = d match {
        case m: DModule => m.getPackages.asScala.toList.flatMap(p => getSupertypes(p))
        case p: DPackage => p.getClasslikes.asScala.toList.flatMap(c => getSupertypes(c))
        case c: DClass => c.get(InheritanceInfo).parents.map(p => (getDRIWithKind(c), getTypeDRI(p))) ++ c.getClasslikes.asScala.toList.flatMap(c => getSupertypes(c))
        case other => List.empty
    }

    private def getDRIWithKind(c: DClass): DRIWithKind = {
        DRIWithKind(c.getDri, c.get(ClasslikeExtension).kind)
    }

    private def getTypeDRI(b: BoundWithKind) = b match {
        case BoundWithKind(t: TypeConstructor, kind: Kind) => DRIWithKind(t.getDri, kind)
        case other => throw IllegalStateException(s"Supertype without DRI: $b")
    }

    private def getSubtypesMap(supertypesList: List[(DRIWithKind, DRIWithKind)]): Map[DRI, List[DRIWithKind]] = supertypesList
        .map( (a,b) => (b,a) )
        .groupBy( (a,b) => a )
        .map{
            case (key, l) => (key.dri, l.map(_(1)))
        }.toMap

    private def completeInheritanceInformation[T <: Documentable](d: T, subtypes: Map[DRI, List[DRIWithKind]]): T = d match {
        case m: DModule => m.copy(
            m.getName,
            m.getPackages.asScala.map(m => completeInheritanceInformation(m,subtypes)).asJava,
            m.getDocumentation,
            m.getExpectPresentInSet,
            m.getSourceSets,
            m.getExtra
        ).asInstanceOf[T]
        case p: DPackage => p.copy(
            p.getDri,
            p.getFunctions,
            p.getProperties,
            p.getClasslikes.asScala.map(c => completeInheritanceInformation(c, subtypes)).asJava,
            p.getTypealiases,
            p.getDocumentation,
            p.getExpectPresentInSet,
            p.getSourceSets,
            p.getExtra
        ).asInstanceOf[T]
        case c: DClass => 
            val newInheritanceInfo = InheritanceInfo(
                c.get(InheritanceInfo).parents,
                subtypes.get(c.getDri).getOrElse(List.empty)
            )
            c.copy(
                c.getDri,
                c.getName,
                c.getConstructors,
                c.getFunctions,
                c.getProperties,
                c.getClasslikes.asScala.map(cl => completeInheritanceInformation(cl,subtypes)).asJava,
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
                modifyExtras(c.getExtra, newInheritanceInfo)
            ).asInstanceOf[T]
        case other => other
    }

    private def modifyExtras(p: PropertyContainer[DClass], i: InheritanceInfo): PropertyContainer[DClass] = 
        val properties = p.getMap.asScala.toMap.filter( (key,value) => key != InheritanceInfo ).map((key, value) => value).asJavaCollection
        PropertyContainer.Companion.empty addAll properties plus i

}