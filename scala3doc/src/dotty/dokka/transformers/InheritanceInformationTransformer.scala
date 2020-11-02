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
    override def invoke(original: DModule, context: DokkaContext): DModule =
        val subtypes = getSupertypes(original).groupBy(_._1).transform((k, v) => v.map(_._2))
        original.updateMembers(m =>  m.withKnownChildren(subtypes.getOrElse(m.dri, Nil)))

    private def getSupertypes(d: Documentable): Seq[(DRI, LinkToType)] = d match {
        case m: DModule => m.getPackages.asScala.toList.flatMap(p => getSupertypes(p))
        case c: Member  =>
            val selfMapping = if !c.kind.isInstanceOf[Classlike] then Nil else
                val selfLink = c.asLink
                c.parents.map(_._2 -> selfLink)

            c.allMembers.flatMap(getSupertypes) ++ selfMapping
        case null => List.empty
    }
