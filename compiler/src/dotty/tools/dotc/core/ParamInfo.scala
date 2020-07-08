package dotty.tools.dotc.core

import Names.Name
import Contexts.{Context, Ctx}
import Types.Type
import Variances.{Variance, varianceToInt}

/** A common super trait of Symbol and LambdaParam.
 *  Used to capture the attributes of type parameters which can be implemented as either.
 */
trait ParamInfo {

  type ThisName <: Name

  /** Is this the info of a type parameter? Will return `false` for symbols
   *  that are not type parameters.
   */
  def isTypeParam: Ctx[Boolean]

  /** The name of the type parameter */
  def paramName: Ctx[ThisName]

  /** The info of the type parameter */
  def paramInfo: Ctx[Type]

  /** The info of the type parameter as seen from a prefix type.
   *  For type parameter symbols, this is the `memberInfo` as seen from `prefix`.
   *  For type lambda parameters, it's the same as `paramInfos` as
   *  `asSeenFrom` has already been applied to the whole type lambda.
   */
  def paramInfoAsSeenFrom(prefix: Type): Ctx[Type]

  /** The parameter bounds, or the completer if the type parameter
   *  is an as-yet uncompleted symbol.
   */
  def paramInfoOrCompleter: Ctx[Type]

  /** The variance of the type parameter */
  def paramVariance: Ctx[Variance]

  /** The variance of the type parameter, as a number -1, 0, +1.
   *  Bivariant is mapped to 1, i.e. it is treated like Covariant.
   */
  final def paramVarianceSign: Ctx[Int] =
    varianceToInt(paramVariance)

  /** A type that refers to the parameter */
  def paramRef: Ctx[Type]
}

object ParamInfo {
  type Of[N] = ParamInfo { type ThisName = N }
}