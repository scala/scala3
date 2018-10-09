package scala.tasty.util

import scala.tasty.Tasty

abstract class Show[T <: Tasty with Singleton](val tasty: T) {

  def showTree(tree: tasty.Tree)(implicit ctx: tasty.Context): String

  def showCaseDef(caseDef: tasty.CaseDef)(implicit ctx: tasty.Context): String

  def showPattern(pattern: tasty.Pattern)(implicit ctx: tasty.Context): String

  def showTypeOrBoundsTree(tpt: tasty.TypeOrBoundsTree)(implicit ctx: tasty.Context): String

  def showTypeOrBounds(tpe: tasty.TypeOrBounds)(implicit ctx: tasty.Context): String

  def showConstant(const: tasty.Constant)(implicit ctx: tasty.Context): String

  def showSymbol(symbol: tasty.Symbol)(implicit ctx: tasty.Context): String

}
