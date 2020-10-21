package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.{Context, ctx}

import transform.DocMiniPhase
import model._
import model.internal._
import model.comment._
import model.references._
import HtmlParsers._
import util.MemberLookup
import util.syntax._

class LinkReturnTypes extends DocMiniPhase with TypeLinker {
  override def transformDef(using Context) = { case df: DefImpl =>
    val returnValue = linkReference(df, df.returnValue, ctx.docbase.packages)
    df.copy(returnValue = returnValue) :: Nil
  }

  override def transformVal(using Context) = { case vl: ValImpl =>
    val returnValue = linkReference(vl, vl.returnValue, ctx.docbase.packages)
    vl.copy(returnValue = returnValue) :: Nil
  }

  override def transformTypeAlias(using Context) = { case ta: TypeAliasImpl =>
    ta.alias.map { alias =>
      val linkedAlias = linkReference(ta, alias, ctx.docbase.packages)
      ta.copy(alias = Some(linkedAlias)) :: Nil
    }
    .getOrElse(ta :: Nil)
  }
}

class LinkParamListTypes extends DocMiniPhase with TypeLinker {
  override def transformDef(using Context) = { case df: DefImpl =>
    val newParamLists = for {
      ParamListImpl(list, isImplicit) <- df.paramLists
      newList = list.map(linkReference(df, _, ctx.docbase.packages))
    } yield ParamListImpl(newList.asInstanceOf[List[NamedReference]].toScalaList, isImplicit)

    df.copy(paramLists = newParamLists) :: Nil
  }
}

class LinkSuperTypes extends DocMiniPhase with TypeLinker {
  def linkSuperTypes(ent: Entity with SuperTypes)(using Context): List[MaterializableLink] =
    ent.superTypes.tolist.collect {
      case UnsetLink(title, query) =>
        handleEntityLink(title, lookup(Some(ent), ctx.docbase.packages, query), ent)
    }

  override def transformClass(using Context) = { case cls: ClassImpl =>
    cls.copy(superTypes = linkSuperTypes(cls).toScalaList) :: Nil
  }

  override def transformCaseClass(using Context) = { case cc: CaseClassImpl =>
    cc.copy(superTypes = linkSuperTypes(cc).toScalaList) :: Nil
  }

  override def transformTrait(using Context) = { case trt: TraitImpl =>
    trt.copy(superTypes = linkSuperTypes(trt).toScalaList) :: Nil
  }

  override def transformObject(using Context) = { case obj: ObjectImpl =>
    obj.copy(superTypes = linkSuperTypes(obj).toScalaList) :: Nil
  }
}

class LinkImplicitlyAddedTypes extends DocMiniPhase with TypeLinker {
  override def transformDef(using Context) = {
    case df: DefImpl if df.implicitlyAddedFrom.isDefined =>
      val implicitlyAddedFrom = linkReference(df, df.implicitlyAddedFrom.get, ctx.docbase.packages)
      df.copy(implicitlyAddedFrom = Some(implicitlyAddedFrom)) :: Nil
  }

  override def transformVal(using Context) = {
    case vl: ValImpl if vl.implicitlyAddedFrom.isDefined =>
      val implicitlyAddedFrom = linkReference(vl, vl.implicitlyAddedFrom.get, ctx.docbase.packages)
      vl.copy(implicitlyAddedFrom = Some(implicitlyAddedFrom)) :: Nil
  }
}

trait TypeLinker extends MemberLookup {
  def handleEntityLink(title: String, target: Option[Entity], ent: Entity, query: String = ""): MaterializableLink =
    target match {
      case Some(target) => new MaterializedLink(title, target)
      case none => NoLink(title, query)
    }

  def linkReference(ent: Entity, ref: Reference, packs: Map[String, Package]): Reference = {
    def linkRef(ref: Reference) = linkReference(ent, ref, packs)

    ref match {
      case ref @ TypeReference(_, UnsetLink(t, query), tps) =>
        val inlineToHtml = InlineToHtml(ent)
        val title = t

        val target = handleEntityLink(title, lookup(Some(ent), packs, query), ent, query)
        val tpTargets = tps.map(linkReference(ent, _, packs))
        ref.copy(tpeLink = target, paramLinks = tpTargets)
      case ref @ OrTypeReference(left, right) =>
        ref.copy(left = linkReference(ent, left, packs), right = linkReference(ent, right, packs))
      case ref @ AndTypeReference(left, right) =>
        ref.copy(left = linkReference(ent, left, packs), right = linkReference(ent, right, packs))
      case ref @ NamedReference(_, rf, _, _) =>
        ref.copy(ref = linkRef(rf))
      case ref @ FunctionReference(args, rv, _) =>
        ref.copy(args = args.map(linkReference(ent, _, packs)), returnValue = linkReference(ent, rv, packs))
      case ref @ TupleReference(args) =>
        ref.copy(args = args.map(linkRef))
      case ref @ BoundsReference(low, high) =>
        ref.copy(low = linkRef(low), high = linkRef(high))
      case _ =>
        ref
    }
  }
}
