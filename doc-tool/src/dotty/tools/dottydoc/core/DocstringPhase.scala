package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context
import dotc.core.Symbols.Symbol
import dotc.core.Comments.{ Comment => CompilerComment }
import transform.DocMiniPhase
import model._
import model.internal._
import model.comment._
import HtmlParsers._
import util.syntax._

import dotty.uoption._

import scala.collection.GenTraversableOnce

/** Phase to add docstrings to the Dottydoc AST */
class DocstringPhase extends DocMiniPhase with CommentParser with CommentCleaner {
  implicit def uOption2GenTraversable[A](uOption: UOption[A]): GenTraversableOnce[A] = uOption.iterator // TODO abstract away
  private def getComment(sym: Symbol)(implicit ctx: Context): UOption[CompilerComment] =
    ctx.docbase.docstring(sym)
    .orElse {
      // If the symbol doesn't have a docstring, look for an overridden
      // ancestor with a docstring
      sym.allOverriddenSymbols.collectFirst {
        case parentSym if ctx.docbase.docstring(parentSym).isDefined =>
          parentSym
      }.toUOption
      .flatMap(ctx.docbase.docstring)
    }

  private def parsedComment(ent: Entity)(implicit ctx: Context): UOption[Comment] =
    getComment(ent.symbol).map { cmt =>
      val parsed = parse(ent, ctx.docbase.packages, clean(cmt.raw), cmt.raw, cmt.pos)

      if (ctx.settings.wikiSyntax.value)
        WikiComment(ent, parsed, cmt.pos).comment
      else
        MarkdownComment(ent, parsed, cmt.pos).comment
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

  override def transformTypeAlias(implicit ctx: Context) = { case ent: TypeAliasImpl =>
    ent.copy(comment = parsedComment(ent))
  }
}
