package dotty.tools.scaladoc
package transformers

class ImplicitMembersExtensionTransformer(using DocContext) extends(Module => Module):
  override def apply(original: Module): Module =
    val classlikeMap = original.members

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

      val MyDri = c.dri
      def collectApplicableMembers(source: Member): Seq[Member] = source.members.flatMap {
        case m @ Member(_, _, _, Kind.Extension(ExtensionTarget(_, _, _, _, MyDri, _), _), Origin.RegularlyDefined) =>
          val kind = m.kind match
            case Kind.Extension(_, d) => d
            case _ => Kind.Def(Nil, Nil)

          Seq(m.withOrigin(Origin.ExtensionFrom(source.name, source.dri)).withKind(kind))
        case m @ Member(_, _, _, conversionProvider: ImplicitConversionProvider, Origin.RegularlyDefined) =>
          conversionProvider.conversion match
            case Some(ImplicitConversion(MyDri, to)) =>
              classlikeMap.get(to).toSeq.flatMap { owner =>
                val newMembers = owner.members.filter(_.origin match
                  case Origin.RegularlyDefined => true
                  case _ => false
                )
                newMembers.map(_.withOrigin(Origin.ImplicitlyAddedBy(m.name, m.dri)))
              }
            case _ =>
              Nil
        case other =>
          None
      }

      val newImplicitMembers = implictSources.flatMap(collectApplicableMembers).distinct
      val expandedMembers = c.members.map(expandMember(outerMembers ++ Seq(c)))
      c.withMembers(newImplicitMembers ++ expandedMembers)

    original.updatePackages(_.map(expandMember(Nil)(_)))
