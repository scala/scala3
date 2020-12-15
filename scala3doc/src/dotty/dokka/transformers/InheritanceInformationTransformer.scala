package dotty.dokka

import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
import org.jetbrains.dokka.model._
import collection.JavaConverters._
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.model.properties._

import dotty.dokka.model._
import dotty.dokka.model.api._


class InheritanceInformationTransformer(val ctx: DokkaContext) extends DocumentableTransformer:
  override def invoke(original: DModule, context: DokkaContext): DModule =
    val subtypes = getSupertypes(original.getPackages.get(0)).groupBy(_._1).transform((k, v) => v.map(_._2))
    original.updateMembers { m =>
      val st: Seq[LinkToType] = subtypes.getOrElse(m.dri, Nil)
      m.withKnownChildren(st).withNewGraphEdges(st.map(_ -> m.asLink))
    }

  private def getSupertypes(c: Member): Seq[(DRI, LinkToType)] =
    val selfMapping =
      if !c.kind.isInstanceOf[Classlike] then Nil
      else c.parents.map(_._2 -> c.asLink)
    c.allMembers.flatMap(getSupertypes) ++ selfMapping
