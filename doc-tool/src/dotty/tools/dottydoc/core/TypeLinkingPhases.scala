package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context
import dotc.util.Positions.NoPosition

import transform.DocMiniPhase
import model._
import model.internal._
import model.comment._
import model.references._
import HtmlParsers._
import util.MemberLookup
import util.traversing._
import util.internal.setters._
import util.syntax._

class LinkReturnTypes extends DocMiniPhase with TypeLinker {
  override def transformDef(implicit ctx: Context) = { case df: DefImpl =>
    val returnValue = linkReference(df, df.returnValue, ctx.docbase.packages)
    df.copy(returnValue = returnValue)
  }

  override def transformVal(implicit ctx: Context) = { case vl: ValImpl =>
    val returnValue = linkReference(vl, vl.returnValue, ctx.docbase.packages)
    vl.copy(returnValue = returnValue)
  }
}

class LinkParamListTypes extends DocMiniPhase with TypeLinker {
  override def transformDef(implicit ctx: Context) = { case df: DefImpl =>
    val newParamLists = for {
      ParamListImpl(list, isImplicit) <- df.paramLists
      newList = list.map(linkReference(df, _, ctx.docbase.packages))
    } yield ParamListImpl(newList.asInstanceOf[List[NamedReference]], isImplicit)

    df.copy(paramLists = newParamLists)
  }
}

class LinkSuperTypes extends DocMiniPhase with TypeLinker {
  def linkSuperTypes(ent: Entity with SuperTypes)(implicit ctx: Context): List[MaterializableLink] =
    ent.superTypes.collect {
      case UnsetLink(title, query) =>
        handleEntityLink(title, lookup(ent, ctx.docbase.packages, query), ent)
    }

  override def transformClass(implicit ctx: Context) = { case cls: ClassImpl =>
    cls.copy(superTypes = linkSuperTypes(cls))
  }

  override def transformCaseClass(implicit ctx: Context) = { case cc: CaseClassImpl =>
    cc.copy(superTypes = linkSuperTypes(cc))
  }

  override def transformTrait(implicit ctx: Context) = { case trt: TraitImpl =>
    trt.copy(superTypes = linkSuperTypes(trt))
  }

  override def transformObject(implicit ctx: Context) = { case obj: ObjectImpl =>
    obj.copy(superTypes = linkSuperTypes(obj))
  }
}

class LinkImplicitlyAddedTypes extends DocMiniPhase with TypeLinker {
  override def transformDef(implicit ctx: Context) = {
    case df: DefImpl if df.implicitlyAddedFrom.isDefined =>
      val implicitlyAddedFrom = linkReference(df, df.implicitlyAddedFrom.get, ctx.docbase.packages)
      df.copy(implicitlyAddedFrom = Some(implicitlyAddedFrom))
  }

  override def transformVal(implicit ctx: Context) = {
    case vl: ValImpl if vl.implicitlyAddedFrom.isDefined =>
      val implicitlyAddedFrom = linkReference(vl, vl.implicitlyAddedFrom.get, ctx.docbase.packages)
      vl.copy(implicitlyAddedFrom = Some(implicitlyAddedFrom))
  }
}

trait TypeLinker extends MemberLookup {
  def handleEntityLink(title: String, target: Option[Entity], ent: Entity, query: String = ""): MaterializableLink =
    target match {
      case Some(target: Package) =>
        MaterializedLink(title, target.path.mkString("/") + "/index.html")
      case Some(target) =>
        MaterializedLink(title, target.path.mkString("/") + ".html")
      case none =>
        NoLink(title, query)
    }

  def linkReference(ent: Entity, ref: Reference, packs: Map[String, Package]): Reference = {
    def linkRef(ref: Reference) = linkReference(ent, ref, packs)

    ref match {
      case ref @ TypeReference(_, UnsetLink(t, query), tps) =>
        val inlineToHtml = InlineToHtml(ent)
        val title = t

        val target = handleEntityLink(title, lookup(ent, packs, query), ent, query)
        val tpTargets = tps.map(linkReference(ent, _, packs))
        ref.copy(tpeLink = target, paramLinks = tpTargets)
      case ref @ OrTypeReference(left, right) =>
        ref.copy(left = linkReference(ent, left, packs), right = linkReference(ent, right, packs))
      case ref @ AndTypeReference(left, right) =>
        ref.copy(left = linkReference(ent, left, packs), right = linkReference(ent, right, packs))
      case ref @ NamedReference(_, rf, _, _) =>
        ref.copy(ref = linkRef(rf))
      case ref @ FunctionReference(args, rv) =>
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
