package dotty.tools.dotc.transform.linker.types

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Types._

class TypeNormalizer(implicit ctx: Context) extends TypeMap {

  override def apply(tp: Type): Type = tp match {
    case tp: TypeRef if tp.typeSymbol.isClass && tp.typeSymbol.isStatic =>
      val sym = tp.typeSymbol
      NamedType(sym.owner.thisType, sym.name, sym.denot)

    case tp: RefinedType =>
      def listRefinements(tp: RefinedType, acc: List[(Name, Type)]): (Type, List[(Name, Type)]) = tp.parent match {
        case p: RefinedType => listRefinements(p, (tp.refinedName, tp.refinedInfo) :: acc)
        case p => (p, (tp.refinedName, tp.refinedInfo) :: acc)
      }
      val (parent, refinements) = listRefinements(tp, Nil)
      val normalizedParent = apply(parent)
      refinements.sortBy(_._1.toString).foldLeft[Type](normalizedParent)((acc, x) => RefinedType(acc, x._1, x._2))

    case _ => mapOver(tp)
  }
}
