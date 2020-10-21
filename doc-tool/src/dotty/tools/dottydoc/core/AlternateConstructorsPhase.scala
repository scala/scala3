package dotty.tools.dottydoc
package core

import dotty.tools.dotc.core.Contexts.{Context, ctx}

import transform.DocMiniPhase
import model._
import model.internal._
import dotty.tools.List.fromIterable

/** This DocMiniPhase adds the alternate constructors, currently defined as
 *  methods with the name `<init>`, to the Entity#constructors list
 */
class AlternateConstructors extends DocMiniPhase {
  def partitionMembers(ent: Entity with Constructors with Members): (List[List[ParamList]], List[Entity]) = {
    val (constructors, members) = ent.members.partition(x => x.name == "<init>")

    val paramLists: List[List[ParamList]] = constructors.collect {
      case df: Def => df.paramLists
    }

    (ent.constructors ++ paramLists, members)
  }

  override def transformClass(using Context) = { case cls: ClassImpl =>
    val (constructors, members) = partitionMembers(cls)
    fromIterable(cls.copy(members = members, constructors = constructors) :: Nil)
  }

  override def transformCaseClass(using Context) = { case cc: CaseClassImpl =>
    val (constructors, members) = partitionMembers(cc)
    fromIterable(cc.copy(members = members, constructors = constructors) :: Nil)
  }
}
