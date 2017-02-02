package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context
import dotc.ast.tpd

import transform.DocMiniPhase
import model.internal._
import model._
import model.factories._
import dotty.tools.dotc.core.Symbols.Symbol
import util.syntax._

class LinkCompanions extends DocMiniPhase {
  private def linkCompanions(ent: Entity)(implicit ctx: Context): ent.type = {
    ent.children.groupBy(_.name).foreach {
      case (_, List(x1: Companion, x2: Companion)) => {
        x1.companionPath = x2.path
        x2.companionPath = x1.path
      }
      case _ => ()
    }
    ent
  }

  override def transformPackage(implicit ctx: Context) = { case ent: PackageImpl =>
    linkCompanions(ent)
  }

  override def transformClass(implicit ctx: Context) = { case ent: ClassImpl =>
    linkCompanions(ent)
  }

  override def transformCaseClass(implicit ctx: Context) = { case ent: CaseClassImpl =>
    linkCompanions(ent)
  }

  override def transformObject(implicit ctx: Context) = { case ent: ObjectImpl =>
    linkCompanions(ent)
  }

  override def transformTrait(implicit ctx: Context) = { case ent: TraitImpl =>
    linkCompanions(ent)
  }
}
