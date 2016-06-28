package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context
import dotc.util.Positions.NoPosition

import transform.DocMiniPhase
import model._
import comment._
import BodyParsers._
import model.internal._
import util.MemberLookup
import util.traversing._
import util.internal.setters._

class ReturnTypeLinker extends DocMiniPhase with TypeLinker {
  override def transformDef(implicit ctx: Context) = { case df: DefImpl =>
    val returnValue = linkReference(df, df.returnValue, ctx.base.packages[Package].toMap)
    df.copy(returnValue = returnValue)
  }

  override def transformVal(implicit ctx: Context) = { case vl: ValImpl =>
    val returnValue = linkReference(vl, vl.returnValue, ctx.base.packages[Package].toMap)
    vl.copy(returnValue = returnValue)
  }
}

class ParamListTypeLinker extends DocMiniPhase with TypeLinker {
  override def transformDef(implicit ctx: Context) = { case df: DefImpl =>
    val newParamLists = for {
      list <- df.paramLists
      newList = list.map(linkReference(df, _, ctx.base.packages[Package].toMap))
    } yield newList.asInstanceOf[List[NamedReference]]

    df.copy(paramLists = newParamLists)
  }
}

trait TypeLinker extends MemberLookup {
  def linkReference(ent: Entity, rv: Reference, packs: Map[String, Package]): Reference =
    rv match {
      case rv @ TypeReference(_, UnsetLink(t, query), tps) =>
        val inlineToHtml = InlineToHtml(ent)
        val title = inlineToHtml(t)

        def handleEntityLink(title: String, lt: LinkTo): MaterializableLink = lt match {
          case Tooltip(str)           => NoLink(title, str)
          case LinkToExternal(_, url) => MaterializedLink(title, url)
          case LinkToEntity(target)   => MaterializedLink(title, util.traversing.relativePath(ent, target))
        }

        val target = handleEntityLink(title, makeEntityLink(ent, packs, t, NoPosition, query).link)

        val tpTargets = tps.map {
          case UnsetLink(t, query) =>
            handleEntityLink(inlineToHtml(t), makeEntityLink(ent, packs, t, NoPosition, query).link)
          case x => x
        }

        rv.copy(tpeLink = target, paramLinks = tpTargets)
      case rv @ OrTypeReference(left, right) =>
        rv.copy(left = linkReference(ent, left, packs), right = linkReference(ent, right, packs))
      case rv @ AndTypeReference(left, right) =>
        rv.copy(left = linkReference(ent, left, packs), right = linkReference(ent, right, packs))
      case rv @ NamedReference(_, ref) => rv.copy(ref = linkReference(ent, ref, packs))
      case _ => rv
    }
}
