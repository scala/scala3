package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context

import transform.DocMiniPhase
import model._
import model.internal._

/** This DocMiniPhase sorts the members of all classes, traits, objects and packages */
class SortMembers extends DocMiniPhase {
  private implicit val EntityOrdering: Ordering[Entity] = new Ordering[Entity] {
    def compare(x: Entity, y: Entity): Int = {
      val nameComp = x.name.compareTo(y.name)
      if (nameComp == 0) x.kind.compareTo(y.kind)
      else nameComp
    }
  }

  private def sort(xs: List[Entity]): List[Entity] = {
    def sortOrNil(xs: Option[List[Entity]]*) =
      xs.map(_.getOrElse(Nil)).reduceLeft(_ ++ _).sorted

    val map = xs.groupBy(_.kind)

    val pkgs = sortOrNil(map.get("package"))
    val typs = sortOrNil(map.get("type"))
    val vals = sortOrNil(map.get("val"), map.get("var"))
    val nested = sortOrNil(map.get("object"), map.get("class"), map.get("case class"), map.get("trait"))
    val defs = sortOrNil(map.get("def"))

    pkgs ++ nested ++ typs ++ vals ++ defs
  }

  override def transformPackage(implicit ctx: Context) = { case p: PackageImpl =>
    p.copy(members = sort(p.members))
  }

  override def transformClass(implicit ctx: Context) = { case c: ClassImpl =>
    c.copy(members = sort(c.members))
  }

  override def transformCaseClass(implicit ctx: Context) = { case cc: CaseClassImpl =>
    cc.copy(members = sort(cc.members))
  }

  override def transformTrait(implicit ctx: Context) = { case t: TraitImpl =>
    t.copy(members = sort(t.members))
  }

  override def transformObject(implicit ctx: Context) = { case o: ObjectImpl =>
    o.copy(members = sort(o.members))
  }
}
