package dotty.dokka

import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
import org.jetbrains.dokka.model._
import collection.JavaConverters
import collection.JavaConverters._
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.model.properties._

import dotty.dokka.model._
import dotty.dokka.model.api._

class ImplicitMembersExtensionTransformer(using DocContext) extends DocumentableTransformer:
  override def invoke(original: DModule, context: DokkaContext): DModule =
    val classlikeMap = original.driMap
    val logger = context.getLogger

    def retrieveCompanion(m: Member) = m.companion.flatMap { dri =>
     val res = classlikeMap.get(dri)
      if res.isEmpty then
        report.warning(s"Companion for class ${m.name} exists but is missing in classlike map")
      res
    }

    def expandMember(outerMembers: Seq[Member])(c: Member): Member =
      val companion = retrieveCompanion(c)

      val allParents = c.parents.flatMap(p => classlikeMap.get(p.dri))

      val parentCompanions = allParents.flatMap(retrieveCompanion)

      // TODO (#220): We can expand this on generic etc
      val implictSources = outerMembers ++ companion.toSeq ++ parentCompanions

      val applicableDRIs = c.parents.map(_.dri).toSet + c.dri

      val MyDri = c.getDri
      def collectApplicableMembers(source: Member): Seq[Member] = source.allMembers.flatMap {
        case m @ Member(_, _, _, Kind.Extension(ExtensionTarget(_, _, MyDri, _), _), Origin.RegularlyDefined) =>
          val kind = m.kind match
            case d: Kind.Def => d
            case _ => Kind.Def(Nil, Nil)

          Seq(m.withOrigin(Origin.ExtensionFrom(source.name, source.dri)).withKind(kind))
        case m @ Member(_, _, _, conversionProvider: ImplicitConversionProvider, Origin.RegularlyDefined) =>
          conversionProvider.conversion match
            case Some(ImplicitConversion(MyDri, to)) =>
              classlikeMap.get(to).toSeq.flatMap { owner =>
                val newMembers = owner.allMembers.filter(_.origin match
                  case Origin.RegularlyDefined => true
                  case _ => false
                )
                newMembers.map(_.withOrigin(Origin.ImplicitlyAddedBy(m.name, m.dri)))
              }
            case _ =>
              Nil
        case _ =>
          None
      }

      val newImplicitMembers = implictSources.flatMap(collectApplicableMembers).distinct
      val expandedMembers = c.allMembers.map(expandMember(newImplicitMembers ++ Seq(c)))
      c.withMembers(newImplicitMembers ++ expandedMembers)

    original.updatePackages(_.map(expandMember(Nil)(_)))
