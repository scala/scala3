package dotty.tools.dotc.transform.linker.summaries

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Types._

/* When source is not None this call was generated as part of a call to source */
class CallInfo(val call: TermRef, val targs: List[Type], val argumentsPassed: List[Type],
    val source: Option[CallInfo])(implicit ctx: Context) extends AbstractCallInfo {

  override def equals(obj: Any): Boolean = obj match {
    case obj: CallInfo =>
      call == obj.call && targs == obj.targs && argumentsPassed == obj.argumentsPassed
    case _ => false
  }

  override def hashCode(): Int = call.hashCode ^ targs.hashCode ^ argumentsPassed.hashCode

}

object CallInfo {

  def apply(call: TermRef, targs: List[Type], argumentsPassed: List[Type], source: Option[CallInfo] = None)(implicit ctx: Context): CallInfo = {
    val normalCall = normilezeType(call).asInstanceOf[TermRef]
    val normalTargs = targs.map(x => normilezeType(x))
    val normalargumentsPassed = argumentsPassed.map(x => normilezeType(x))
    new CallInfo(normalCall, normalTargs, normalargumentsPassed, source)
  }

  private def normilezeType(tp: Type)(implicit ctx: Context): Type = new TypeNormilizer().apply(tp)

  private class TypeNormilizer(implicit ctx: Context) extends TypeMap {

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
}
