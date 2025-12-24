package scala.quoted.compiletime.internal

import dotty.tools.dotc.ast.tpd
import scala.annotation.experimental
import scala.quoted.compiletime as pub

/////// ParamClause ///////////////////////////////////////////////////////////////

type ParamClause = pub.ParamClause & ParamClauseImpl
sealed trait ParamClauseImpl { _self: pub.ParamClause =>
  val underlying: tpd.ParamClause
}
object ParamClauseImpl {
  def apply(x: tpd.ParamClause): ParamClauseImpl = x match {
    case x: List[tpd.ValDef] @unchecked if x.isEmpty || x.head.isInstanceOf[tpd.ValDef] => new TermParamClauseImpl(x)
    case x: List[tpd.TypeDef] @unchecked if x.isEmpty || x.head.isInstanceOf[tpd.TypeDef] => new TypeParamClauseImpl(x)
    case _ => throw new MatchError(x)
  }
  object Module extends pub.ParamClause.Module {}
}

/////// TermParamClause ///////////////////////////////////////////////////////////////

type TermParamClause = TermParamClauseImpl
final class TermParamClauseImpl(val underlying: List[tpd.ValDef]) extends ParamClauseImpl, pub.TermParamClause {
  override def params: List[pub.ValDef] = ???
  override def isImplicit: Boolean = ???
  override def isGiven: Boolean = ???
  @experimental override def erasedArgs: List[Boolean] = ???
  @experimental override def hasErasedArgs: Boolean = ???
}
object TermParamClauseImpl {
  object Module extends pub.TermParamClause.Module {
    override def apply(params: List[pub.ValDef]): pub.TermParamClause = ???
    override def make(params: List[pub.ValDef]): pub.TermParamClause = ???
  }
}

/////// TypeParamClause ///////////////////////////////////////////////////////////////

type TypeParamClause = TypeParamClauseImpl
final class TypeParamClauseImpl(val underlying: List[tpd.TypeDef]) extends ParamClauseImpl, pub.TypeParamClause {
  override def params: List[pub.TypeDef] = ???
}
object TypeParamClauseImpl {
  object Module extends pub.TypeParamClause.Module {
    override def apply(params: List[pub.TypeDef]): pub.TypeParamClause = ???
    override def make(params: List[pub.TypeDef]): pub.TypeParamClause = ???
  }
}