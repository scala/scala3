package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context
import transform.DocMiniPhase
import model._
import model.internal._
import model.comment._
import BodyParsers._

class DocstringPhase extends DocMiniPhase with CommentParser with CommentCleaner {
  private def parsedComment[E <: Entity](ent: E)(implicit ctx: Context): Option[Comment] =
    ctx.docbase.docstring(ent.symbol).map { cmt =>
       parse(ent, ctx.docbase.packages[Package].toMap, clean(cmt.raw), cmt.raw, cmt.pos)
        .toComment(_.toHtml(ent))
    }

  override def transformPackage(implicit ctx: Context) = { case ent: PackageImpl =>
    ent.copy(comment = parsedComment(ent))
  }

  override def transformClass(implicit ctx: Context) = { case ent: ClassImpl =>
    ent.copy(comment = parsedComment(ent))
  }

  override def transformCaseClass(implicit ctx: Context) = { case ent: CaseClassImpl =>
    ent.copy(comment = parsedComment(ent))
  }

  override def transformTrait(implicit ctx: Context) = { case ent: TraitImpl =>
    ent.copy(comment = parsedComment(ent))
  }

  override def transformObject(implicit ctx: Context) = { case ent: ObjectImpl =>
    ent.copy(comment = parsedComment(ent))
  }

  override def transformDef(implicit ctx: Context) = { case ent: DefImpl =>
    ent.copy(comment = parsedComment(ent))
  }

  override def transformVal(implicit ctx: Context) = { case ent: ValImpl =>
    ent.copy(comment = parsedComment(ent))
  }
}
