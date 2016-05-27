package dotty.tools.dotc.core

import Names.Name
import Contexts.Context
import Types.Type

/** A common super trait of Symbol and Refinement.
 *  Used to capture the attributes of type parameters
 *  which can be implemented as either symbols or refinements.
 */
trait MemberBinding {

  /** Does this binding represent a type parameter?
   *  Only in that case the rest of the binding's methods are significant.
   */
  def isTypeParam(implicit ctx: Context): Boolean

  /** The name of the member */
  def memberName(implicit ctx: Context): Name

  /** The info of the member */
  def memberInfo(implicit ctx: Context): Type

  /** The info of the member as seen from a prefix type.
   *  This can be different from `memberInfo` if the binding
   *  is a type symbol of a class.
   */
  def memberInfoAsSeenFrom(pre: Type)(implicit ctx: Context): Type

  /** The variance of the type parameter
   *  @pre: isTypeParam = true
   */
  def memberVariance(implicit ctx: Context): Int
}