package dotty.tools.dotc.core

import Names.Name
import Contexts.Context
import Types.Type

/** The common info associated with a member symbol and a refinement */
trait MemberInfo {

  def exists(implicit ctx: Context): Boolean

  def memberName(implicit ctx: Context): Name

  def memberInfo(implicit ctx: Context): Type

  def memberInfoAsSeenFrom(pre: Type)(implicit ctx: Context): Type

  def memberVariance(implicit ctx: Context): Int


}