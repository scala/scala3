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

  override def transformDef(implicit ctx: Context) = { case df: DefImpl =>
    ctx.docbase.docstring(df.symbol).flatMap(_.usecases.headOption.map(_.tpdCode)).map(defdefToDef(_, df.symbol)).getOrElse(df)
  }
}
