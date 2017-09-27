package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context
import dotc.ast.tpd

import transform.DocMiniPhase
import model.internal._
import model.factories._
import dotty.tools.dotc.core.Symbols.Symbol
import util.syntax._

import dotty.uoption._
import scala.collection.GenTraversableOnce

class UsecasePhase extends DocMiniPhase {
  private def defdefToDef(d: tpd.DefDef, sym: Symbol)(implicit ctx: Context) = {
    val name = d.name.show.split("\\$").head // UseCase defs get $pos appended to their names
    DefImpl(
      sym,
      annotations(sym),
      name,
      flags(d),
      path(d.symbol).init :+ name,
      returnType(d.tpt.tpe),
      typeParams(d.symbol),
      paramLists(d.symbol.info)
    )
  }

  implicit def uOption2GenTraversable[A](uOption: UOption[A]): GenTraversableOnce[A] = uOption.iterator // TODO abstract away
  override def transformDef(implicit ctx: Context) = { case df: DefImpl =>
    ctx.docbase.docstring(df.symbol).flatMap(_.usecases.headOption.toUOption.map(_.tpdCode)).map(defdefToDef(_, df.symbol)).getOrElse(df)
  }
}
