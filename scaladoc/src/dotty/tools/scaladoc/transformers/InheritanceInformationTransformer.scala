package dotty.tools.scaladoc
package transformers

class InheritanceInformationTransformer(using DocContext) extends (Module => Module):
  override def apply(original: Module): Module =
    val subtypes = getSupertypes(original.rootPackage).groupMap(_(0))(_(1))
    original.updateMembers { m =>
      val edges = getEdges(m.asLink.copy(kind = bareClasslikeKind(m.kind)), subtypes)
      val st: Seq[LinkToType] = edges.map(_._1).distinct
      m.withKnownChildren(st).withNewGraphEdges(edges)
    }

  private def getEdges(ltt: LinkToType, subtypes: Map[DRI, Seq[LinkToType]]): Seq[(LinkToType, LinkToType)] =
    val st: Seq[LinkToType] = subtypes.getOrElse(ltt.dri, Nil)
    st.flatMap(s => Seq(s -> ltt) ++ getEdges(s, subtypes))

  private def bareClasslikeKind(kind: Kind): Kind = kind match
    case _: Kind.Trait => Kind.Trait(Nil, Nil)
    case _: Kind.Class => Kind.Class(Nil, Nil)
    case e if e.isInstanceOf[Kind.Enum] => Kind.Enum(Nil, Nil)
    case ec if ec.isInstanceOf[Kind.EnumCase] => Kind.EnumCase(Kind.Object)
    case o => o

  private def getSupertypes(c: Member): Seq[(DRI, LinkToType)] =
    val selfMapping =
      if !c.kind.isInstanceOf[Classlike] && !c.kind.isInstanceOf[Kind.EnumCase] then Nil
      else c.directParents.map(p => p.dri -> c.asLink)
    c.members.flatMap(getSupertypes) ++ selfMapping
