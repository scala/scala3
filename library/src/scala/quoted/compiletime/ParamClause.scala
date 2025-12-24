package scala.quoted.compiletime

import scala.annotation.experimental

/////// ParamClause ///////////////////////////////////////////////////////////////

/**
  * A parameter clause `[X1, ..., Xn]` or `(x1: X1, ..., xn: Xx)`
  *
  *  `[X1, ..., Xn]` are represented with `TypeParamClause` and `(x1: X1, ..., xn: Xx)` are represented with `TermParamClause`
  *
  *  `ParamClause` encodes the following enumeration
  *  ```scala
  *  //{
  *  import scala.quoted.*
  *  def inQuotes(using Quotes) = {
  *    val q: Quotes = summon[Quotes]
  *    import q.reflect.*
  *  //}
  *    enum ParamClause:
  *      case TypeParamClause(params: List[TypeDef])
  *      case TermParamClause(params: List[ValDef])
  *  //{
  *  }
  *  //}
  *  ```
  */
sealed trait ParamClause private[compiletime] () {

  /** List of parameters of the clause. */
  def params: List[ValDef] | List[TypeDef]
}
object ParamClause {

  def quoted(using quotes: Quotes): ParamClause.Module = quotes.reflectV2.ParamClause
  given moduleConversion: (quotes: Quotes) => Conversion[ParamClause.type, ParamClause.Module] = _ => quotes.reflectV2.ParamClause

  trait Module private[compiletime] () {}

}

/////// TermParamClause ///////////////////////////////////////////////////////////////

/**
  * A term parameter clause `(x1: X1, ..., xn: Xx)`
  *  Can also be `(implicit X1, ..., Xn)`, `(given X1, ..., Xn)` or `(given x1: X1, ..., xn: Xn)`
  */
trait TermParamClause private[compiletime] () extends ParamClause {

  /** List of parameters of the clause. */
  def params: List[ValDef]

  /** Is this an implicit parameter clause `(implicit x1: X1, ..., xn: Xn)`. */
  def isImplicit: Boolean

  /** Is this a given parameter clause `(using X1, ..., Xn)` or `(using x1: X1, ..., xn: Xn)`. */
  def isGiven: Boolean

  /** List of `erased` flags for each parameter of the clause. */
  @experimental
  def erasedArgs: List[Boolean]

  /** Whether the clause has any erased parameters. */
  @experimental
  def hasErasedArgs: Boolean

}
object TermParamClause {

  def quoted(using quotes: Quotes): TermParamClause.Module = quotes.reflectV2.TermParamClause
  given moduleConversion: (quotes: Quotes) => Conversion[TermParamClause.type, TermParamClause.Module] = _ => quotes.reflectV2.TermParamClause

  def unapply(x: TermParamClause): Some[List[ValDef]] = Some(x.params)

  trait Module private[compiletime] () {
    def apply(params: List[ValDef]): TermParamClause
    def make(params: List[ValDef]): TermParamClause
  }

}

/////// TypeParamClause ///////////////////////////////////////////////////////////////

/** A type parameter clause `[X1, ..., Xn]`. */
trait TypeParamClause private[compiletime] () extends ParamClause {

  /** List of parameters of the clause. */
  def params: List[TypeDef]

}
object TypeParamClause {

  def quoted(using quotes: Quotes): TypeParamClause.Module = quotes.reflectV2.TypeParamClause
  given moduleConversion: (quotes: Quotes) => Conversion[TypeParamClause.type, TypeParamClause.Module] = _ => quotes.reflectV2.TypeParamClause

  def unapply(x: TypeParamClause): Some[List[TypeDef]] = Some(x.params)

  trait Module private[compiletime] () {
    def apply(params: List[TypeDef]): TypeParamClause
    def make(params: List[TypeDef]): TypeParamClause
  }

}
