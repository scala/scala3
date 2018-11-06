package scala.tasty.util

import scala.tasty.Reflection

abstract class Show[R <: Reflection with Singleton](val reflect: R) {

  def showTree(tree: reflect.Tree)(implicit ctx: reflect.Context): String

  def showCaseDef(caseDef: reflect.CaseDef)(implicit ctx: reflect.Context): String

  def showPattern(pattern: reflect.Pattern)(implicit ctx: reflect.Context): String

  def showTypeOrBoundsTree(tpt: reflect.TypeOrBoundsTree)(implicit ctx: reflect.Context): String

  def showTypeOrBounds(tpe: reflect.TypeOrBounds)(implicit ctx: reflect.Context): String

  def showConstant(const: reflect.Constant)(implicit ctx: reflect.Context): String

  def showSymbol(symbol: reflect.Symbol)(implicit ctx: reflect.Context): String

}
