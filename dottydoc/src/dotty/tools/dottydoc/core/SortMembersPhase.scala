package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context

import transform.DocMiniPhase
import model._
import model.internal._

/** This DocMiniPhase sorts the members of all classes, traits, objects and packages */
class SortMembers extends DocMiniPhase {
  override def transformPackage(implicit ctx: Context) = { case p: PackageImpl =>
    p.copy(members = p.members.sortBy(_.name))
  }

  override def transformClass(implicit ctx: Context) = { case c: ClassImpl =>
    c.copy(members = c.members.sortBy(_.name))
  }

  override def transformCaseClass(implicit ctx: Context) = { case cc: CaseClassImpl =>
    cc.copy(members = cc.members.sortBy(_.name))
  }

  override def transformTrait(implicit ctx: Context) = { case t: TraitImpl =>
    t.copy(members = t.members.sortBy(_.name))
  }

  override def transformObject(implicit ctx: Context) = { case o: ObjectImpl =>
    o.copy(members = o.members.sortBy(_.name))
  }
}
