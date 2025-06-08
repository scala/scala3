package scala.quoted.meta

import scala.annotation.experimental

sealed trait ParamClause {

  /** List of parameters of the clause */
  def params: List[ValDef] | List[TypeDef]

}
object ParamClause {

  def api(using meta: Meta): Meta.ParamClauseAPI = meta.internal.paramClause
  given Meta => Conversion[ParamClause.type, Meta.ParamClauseAPI] = _.api

}

trait TermParamClause private[meta] extends ParamClause {

  /** List of parameters of the clause */
  def params: List[ValDef]

  /** Is this an implicit parameter clause `(implicit x1: X1, ..., xn: Xn)` */
  def isImplicit: Boolean

  /** Is this a given parameter clause `(using X1, ..., Xn)` or `(using x1: X1, ..., xn: Xn)` */
  def isGiven: Boolean

  /** List of `erased` flags for each parameter of the clause */
  @experimental
  def erasedArgs: List[Boolean]

  /** Whether the clause has any erased parameters */
  @experimental
  def hasErasedArgs: Boolean

}
object TermParamClause {

  def api(using meta: Meta): Meta.TermParamClauseAPI = meta.internal.termParamClause
  given Meta => Conversion[TermParamClause.type, Meta.TermParamClauseAPI] = _.api

  def unapply(x: TermParamClause): Some[List[ValDef]] = Some(x.params)

}

trait TypeParamClause private[meta] extends ParamClause {

  /** List of parameters of the clause */
  def params: List[TypeDef]

}
object TypeParamClause {

  def api(using meta: Meta): Meta.TypeParamClauseAPI = meta.internal.typeParamClause
  given Meta => Conversion[TypeParamClause.type, Meta.TypeParamClauseAPI] = _.api

  def unapply(x: TypeParamClause): Some[List[TypeDef]] = Some(x.params)

}
