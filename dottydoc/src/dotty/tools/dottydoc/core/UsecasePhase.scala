package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context
import dotc.ast.tpd

import transform.DocMiniPhase
import model.internal._
import model.factories._
import dotty.tools.dotc.core.Symbols.Symbol

class UsecasePhase extends DocMiniPhase {
  private def defdefToDef(d: tpd.DefDef, sym: Symbol)(implicit ctx: Context) = DefImpl(
    sym,
    d.name.show.split("\\$").head, // UseCase defs get $pos appended to their names
    flags(d), path(d.symbol),
    returnType(d.tpt.tpe),
    typeParams(d.symbol),
    paramLists(d.symbol.info)
  )

  override def transformDef(implicit ctx: Context) = { case df: DefImpl =>
    ctx.docbase.docstring(df.symbol).flatMap(_.usecases.headOption.map(_.tpdCode)).map(defdefToDef(_, df.symbol)).getOrElse(df)
  }
}
