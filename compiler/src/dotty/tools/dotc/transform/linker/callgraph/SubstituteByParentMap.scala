package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.{ClassInfo, DeepTypeMap, HKApply, NoPrefix, RefinedType, TermType, Type, TypeAlias, TypeRef}

class SubstituteByParentMap(substMap: OuterTargs)(implicit ctx: Context) extends DeepTypeMap()(ctx) {

  def apply(tp: Type): Type = {
    lazy val substitution = substMap.mp.getOrElse(tp.typeSymbol.owner, Nil)
    def termTypeIfNeed(t: Type): Type = {
      if (tp.isInstanceOf[TermType] && !t.isInstanceOf[TermType]) {
        t match {
          case t: TypeAlias =>
            assert(t.underlying.isInstanceOf[TermType])
            t.underlying
          case t: ClassInfo =>
            t.typeRef
          case _ =>
            assert(false)
            null
        }
      } else t
    }
    tp match {
      case tp: RefinedType => mapOver(tp) // otherwise we will loose refinement
      case tp: TypeAlias => mapOver(tp) // map underlying
      case tp: HKApply => mapOver(tp)
      case _ if tp.typeSymbol.exists && substitution.nonEmpty =>
        var typ = tp
        /*val id = tp.typeSymbol.owner.info match {
          case t: PolyType =>
            t.paramNames.indexOf(tp.typeSymbol.name)
          case t: ClassInfo =>
            var typ = tp
            var id = t.typeParams.indexOf(typ.typeSymbol)
            while (id < 0 && (tp.typeSymbol.info.typeSymbol ne tp.typeSymbol)) {
              typ = tp.typeSymbol.info
              id = t.typeParams.indexOf(typ.typeSymbol)
            }
            id
          case _ =>
            -2
        } */
        var id = substitution.find(x => x._1 == tp.typeSymbol.name)
        var limit = 30
        var stack: List[Type] = Nil
        while (id.isEmpty && (limit > 0) && (typ.typeSymbol.info.typeSymbol ne typ.typeSymbol)) {
          typ = typ.typeSymbol.info
          stack = typ :: stack
          id = substitution.find(x => x._1 == typ.typeSymbol.name)
          limit -= 1
        }

        // assert(id.isDefined)
        if (id.isDefined) {
          val t = termTypeIfNeed(id.get._2.stripTypeVar)
          if (!(t =:= typ))
            apply(termTypeIfNeed(t))
          else t
        } else tp
      case t: TypeRef if t.prefix.normalizedPrefix eq NoPrefix =>
        val tmp = apply(t.info)
        if (tmp ne t.info) termTypeIfNeed(tmp)
        else mapOver(t)
      case _ => mapOver(tp)

    }

  }
}
