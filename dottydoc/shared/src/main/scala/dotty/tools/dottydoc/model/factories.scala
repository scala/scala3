package dotty.tools.dottydoc
package model

import dotty.tools.dotc
import dotc.core.Types._
import dotc.core.Contexts.Context
import dotc.core.Symbols.Symbol
import dotc.core.{ Flags => DottyFlags }
import dotc.ast.Trees._
import DottyFlags.FlagSet

object factories {
  import dotty.tools.dotc.ast.tpd._
  import DottyFlags._

  def flags(t: Tree)(implicit ctx: Context): List[String] =
    (t.symbol.flags & SourceModifierFlags).flagStrings.toList

  def path(t: Tree)(implicit ctx: Context): List[String] = {
    def pathList(tpe: Type): List[String] = tpe match {
      case t: ThisType =>
        pathList(t.tref)
      case t: NamedType if t.prefix == NoPrefix  && t.name.toString == "<root>" =>
        Nil
      case t: NamedType if t.prefix == NoPrefix =>
        t.name.toString :: Nil
      case t: NamedType =>
        pathList(t.prefix) :+ t.name.toString
    }

    val ref =
      if (t.symbol.isTerm) t.symbol.termRef
      else t.symbol.typeRef

    pathList(ref)
  }

  // TODO: should be updated to link to local entities
  def returnType(t: Tree)(implicit ctx: Context): String =
    t.show
}
