package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.ContextRenamed

import transform.DocMiniPhase
import model.internal._
import model._

class LinkCompanions extends DocMiniPhase {
  private def linkCompanions(ent: Entity)(implicit ctx: ContextRenamed): ent.type = {
    ent.children.groupBy(_.name).foreach {
      case (_, List(x1: Companion, x2: Companion)) =>
        x1.companionPath = x2.path
        x2.companionPath = x1.path

      case _ => ()
    }
    ent
  }

  override def transformPackage(implicit ctx: ContextRenamed) = { case ent: PackageImpl =>
    linkCompanions(ent) :: Nil
  }

  override def transformClass(implicit ctx: ContextRenamed) = { case ent: ClassImpl =>
    linkCompanions(ent) :: Nil
  }

  override def transformCaseClass(implicit ctx: ContextRenamed) = { case ent: CaseClassImpl =>
    linkCompanions(ent) :: Nil
  }

  override def transformObject(implicit ctx: ContextRenamed) = { case ent: ObjectImpl =>
    linkCompanions(ent) :: Nil
  }

  override def transformTrait(implicit ctx: ContextRenamed) = { case ent: TraitImpl =>
    linkCompanions(ent) :: Nil
  }
}
