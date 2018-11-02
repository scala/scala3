package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.ContextRenamed
import dotc.core.Symbols.Symbol
import dotc.core.Comments.{ Comment => CompilerComment }
import transform.DocMiniPhase
import model._
import model.internal._
import model.comment._
import util.syntax._

/** Phase to add docstrings to the Dottydoc AST */
class DocstringPhase extends DocMiniPhase with CommentParser with CommentCleaner {

  private def getComment(sym: Symbol)(implicit ctx: ContextRenamed): Option[CompilerComment] = {
    ctx.docbase.docstring(sym)
    .orElse {
      // If the symbol doesn't have a docstring, look for an overridden
      // ancestor with a docstring
      sym.allOverriddenSymbols.collectFirst {
        case parentSym if ctx.docbase.docstring(parentSym).isDefined =>
          parentSym
      }
      .flatMap(ctx.docbase.docstring)
    }
  }

  private def parsedComment(ent: Entity)(implicit ctx: ContextRenamed): Option[Comment] = {
    for {
      comment <- getComment(ent.symbol)
      text <- comment.expandedBody
    } yield {
      val parsed = parse(ent, ctx.docbase.packages, clean(text), text, comment.pos)
      if (ctx.settings.wikiSyntax.value)
        WikiComment(ent, parsed, comment.pos).comment
      else
        MarkdownComment(ent, parsed, comment.pos).comment
    }
  }

  override def transformPackage(implicit ctx: ContextRenamed) = { case ent: PackageImpl =>
    ent.copy(comment = parsedComment(ent)) :: Nil
  }

  override def transformClass(implicit ctx: ContextRenamed) = { case ent: ClassImpl =>
    ent.copy(comment = parsedComment(ent)) :: Nil
  }

  override def transformCaseClass(implicit ctx: ContextRenamed) = { case ent: CaseClassImpl =>
    ent.copy(comment = parsedComment(ent)) :: Nil
  }

  override def transformTrait(implicit ctx: ContextRenamed) = { case ent: TraitImpl =>
    ent.copy(comment = parsedComment(ent)) :: Nil
  }

  override def transformObject(implicit ctx: ContextRenamed) = { case ent: ObjectImpl =>
    ent.copy(comment = parsedComment(ent)) :: Nil
  }

  override def transformDef(implicit ctx: ContextRenamed) = { case ent: DefImpl =>
    ent.copy(comment = parsedComment(ent)) :: Nil
  }

  override def transformVal(implicit ctx: ContextRenamed) = { case ent: ValImpl =>
    ent.copy(comment = parsedComment(ent)) :: Nil
  }

  override def transformTypeAlias(implicit ctx: ContextRenamed) = { case ent: TypeAliasImpl =>
    ent.copy(comment = parsedComment(ent)) :: Nil
  }
}
