package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Types

import scala.tasty.types

object LambdaType {

  def apply(tpe: Types.LambdaType): types.LambdaType[_, _] = tpe match {
    case tpe: Types.MethodType => MethodType(tpe)
    case tpe: Types.PolyType => PolyType(tpe)
    case tpe: Types.TypeLambda => TypeLambda(tpe)
  }

}
