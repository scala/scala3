package dotty.tools.dotc.core

import Names.TypeName
import Contexts.Context
import Types.{Type, TypeBounds}

/** A common super trait of Symbol and LambdaParam.
 *  Used to capture the attributes of type parameters which can be implemented as either.
 */
trait ParamInfo {

  /** Is this the info of a type parameter? Will return `false` for symbols
   *  that are not type parameters.
   */
  def isTypeParam(implicit ctx: Context): Boolean

  /** The name of the type parameter */
  def paramName(implicit ctx: Context): TypeName

  /** The info of the type parameter */
  def paramInfo(implicit ctx: Context): Type

  /** The info of the type parameter as seen from a prefix type.
   *  For type parameter symbols, this is the `memberInfo` as seen from `prefix`.
   *  For type lambda parameters, it's the same as `paramBounds` as
   *  `asSeenFrom` has already been applied to the whole type lambda.
   */
  def paramInfoAsSeenFrom(pre: Type)(implicit ctx: Context): Type

  /** The parameter bounds, or the completer if the type parameter
   *  is an as-yet uncompleted symbol.
   */
  def paramInfoOrCompleter(implicit ctx: Context): Type

  /** The variance of the type parameter */
  def paramVariance(implicit ctx: Context): Int

  /** A type that refers to the parameter */
  def paramRef(implicit ctx: Context): Type
}