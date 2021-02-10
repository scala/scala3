package dotty.tools.scaladoc
package transformers


class SealedMarksGraphTransformer(using context: DocContext) extends (Module => Module):
  override def apply(original: Module): Module =
    val sealedRelations = getSealedRelations(original.rootPackage)
    original.updateMembers { m =>
      m.copy(graph = m.graph.copy(sealedNodes = m.graph.vertecies.filter { v =>
        sealedRelations.contains(v.dri)
      }.toSet))
    }

  private def getSealedRelations(c: Member): Set[DRI] =
    val selfMapping = if c.modifiers.contains(Modifier.Sealed) then Set(c.dri) else Set.empty
    selfMapping ++ c.members.flatMap(getSealedRelations)
