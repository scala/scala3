package dotty.tools.dotc.core

import Names.Name
import Contexts.ContextRenamed
import Types.Type

/** A common super trait of Symbol and LambdaParam.
 *  Used to capture the attributes of type parameters which can be implemented as either.
 */
trait ParamInfo {

  type ThisName <: Name

  /** Is this the info of a type parameter? Will return `false` for symbols
   *  that are not type parameters.
   */
  def isTypeParam(implicit ctx: ContextRenamed): Boolean

  /** The name of the type parameter */
  def paramName(implicit ctx: ContextRenamed): ThisName

  /** The info of the type parameter */
  def paramInfo(implicit ctx: ContextRenamed): Type

  /** The info of the type parameter as seen from a prefix type.
   *  For type parameter symbols, this is the `memberInfo` as seen from `prefix`.
   *  For type lambda parameters, it's the same as `paramInfos` as
   *  `asSeenFrom` has already been applied to the whole type lambda.
   */
  def paramInfoAsSeenFrom(prefix: Type)(implicit ctx: ContextRenamed): Type

  /** The parameter bounds, or the completer if the type parameter
   *  is an as-yet uncompleted symbol.
   */
  def paramInfoOrCompleter(implicit ctx: ContextRenamed): Type

  /** The variance of the type parameter */
  def paramVariance(implicit ctx: ContextRenamed): Int

  /** A type that refers to the parameter */
  def paramRef(implicit ctx: ContextRenamed): Type
}

object ParamInfo {
  type Of[N] = ParamInfo { type ThisName = N }
}