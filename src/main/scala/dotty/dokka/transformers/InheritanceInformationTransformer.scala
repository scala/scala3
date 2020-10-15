package dotty.dokka

import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.properties._

import dotty.dokka.model._
import dotty.dokka.model.api._


class InheritanceInformationTransformer(val ctx: DokkaContext) extends DocumentableTransformer:
    override def invoke(original: DModule, context: DokkaContext): DModule = {
        val subtypes = getSupertypes(original).groupBy(_._1).transform((k, v) => v.map(_._2))
        completeInheritanceInformation(subtypes)(original)
    }

private def getSupertypes(d: Documentable): Seq[(DRI, LinkToType)] = d match {
        case m: DModule => m.getPackages.asScala.toList.flatMap(p => getSupertypes(p))
        case p: DPackage => p.getClasslikes.asScala.toList.flatMap(c => getSupertypes(c))
        case c: DClass => 
            val selfLink = c.asLink
            c.parents.map(_._2 -> selfLink) ++ c.getClasslikes.asScala.flatMap(getSupertypes)
        case other => List.empty
    }

    private def completeInheritanceInformation[T <: Documentable](subtypes: Map[DRI, Seq[LinkToType]])(d: T): T = (d match {
        case m: DModule => 
            m.updatePackanges(_.map(completeInheritanceInformation(subtypes)))
        
        case p: DPackage => 
            p.updateClasslikes(_.map(completeInheritanceInformation(subtypes)))
        
        case c: DClass => 
            c.updateClasslikes(_.map(completeInheritanceInformation(subtypes))).withKnownChildren(subtypes.getOrElse(c.dri, Nil))
        case other => other
    }).asInstanceOf[T]
