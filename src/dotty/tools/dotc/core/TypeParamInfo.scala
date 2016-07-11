package dotty.tools.dotc.core

import Names.Name
import Contexts.Context
import Types.{Type, TypeBounds}

/** A common super trait of Symbol and LambdaParam.
 *  Used to capture the attributes of type parameters which can be implemented as either.
 */
trait TypeParamInfo {

  /** Is this the info of a type parameter? Might be wrong for symbols */
  def isTypeParam(implicit ctx: Context): Boolean

  /** The name of the type parameter */
  def paramName(implicit ctx: Context): Name

  /** The info of the type parameter */
  def paramBounds(implicit ctx: Context): TypeBounds

  /** The info of the type parameter as seen from a prefix type.
   *  This can be different from `memberInfo` if the binding
   *  is a type symbol of a class.
   */
  def paramBoundsAsSeenFrom(pre: Type)(implicit ctx: Context): TypeBounds

  /** The variance of the type parameter */
  def paramVariance(implicit ctx: Context): Int

  /** A type that refers to the parameter */
  def paramRef(implicit ctx: Context): Type
}