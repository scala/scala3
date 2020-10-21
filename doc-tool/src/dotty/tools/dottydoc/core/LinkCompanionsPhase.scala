package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.{Context, ctx}

import transform.DocMiniPhase
import model.internal._
import model._

class LinkCompanions extends DocMiniPhase {
  private def linkCompanions(ent: Entity)(using Context): ent.type = {
    ent.children.groupBy(_.name).foreach {
      case (_, scala.List(x1: Companion, x2: Companion)) =>
        x1.companionPath = x2.path
        x2.companionPath = x1.path

      case _ => ()
    }
    ent
  }

  override def transformPackage(using Context) = { case ent: PackageImpl =>
    linkCompanions(ent) :: Nil
  }

  override def transformClass(using Context) = { case ent: ClassImpl =>
    linkCompanions(ent) :: Nil
  }

  override def transformCaseClass(using Context) = { case ent: CaseClassImpl =>
    linkCompanions(ent) :: Nil
  }

  override def transformObject(using Context) = { case ent: ObjectImpl =>
    linkCompanions(ent) :: Nil
  }

  override def transformTrait(using Context) = { case ent: TraitImpl =>
    linkCompanions(ent) :: Nil
  }
}
