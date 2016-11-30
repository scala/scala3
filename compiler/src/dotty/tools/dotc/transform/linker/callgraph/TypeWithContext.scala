package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Types.Type

import scala.collection.mutable

class TypeWithContext(val tp: Type, val outerTargs: OuterTargs) {
  val castsCache: mutable.Set[Cast] = mutable.Set.empty

  override def hashCode(): Int = tp.hashCode() * 31 + outerTargs.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case t: TypeWithContext => t.tp.equals(tp) && (t.outerTargs equals outerTargs)
    case _ => false
  }

  override def toString: String = s"TypeWithContext($tp, $outerTargs)"
}
