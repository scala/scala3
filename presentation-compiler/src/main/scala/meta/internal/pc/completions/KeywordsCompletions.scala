package scala.meta.internal.pc.completions

import scala.meta.internal.mtags.MtagsEnrichments.given
import scala.meta.internal.pc.Keyword
import scala.meta.internal.pc.KeywordCompletionsUtils
import scala.meta.tokenizers.XtensionTokenizeInputLike
import scala.meta.tokens.Token

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourcePosition

object KeywordsCompletions:

  def contribute(
      path: List[Tree],
      completionPos: CompletionPos,
  )(using ctx: Context): List[CompletionValue] =
    lazy val notInComment = checkIfNotInComment(completionPos.cursorPos, path)
    path match
      case Nil if completionPos.query.isEmpty =>
        Keyword.all.collect {
          // topelevel definitions are allowed in Scala 3
          case kw if (kw.isPackage || kw.isTemplate) && notInComment =>
            CompletionValue.keyword(kw.name, kw.insertText)
        }
      case _ =>
        val isExpression = this.isExpression(path)
        val isBlock = this.isBlock(path)
        val isDefinition =
          this.isDefinition(path, completionPos.query, completionPos.cursorPos)
        val isMethodBody = this.isMethodBody(path)
        val isTemplate = this.isTemplate(path)
        val isPackage = this.isPackage(path)
        val isParam = this.isParam(path)
        val isSelect = this.isSelect(path)
        val isImport = this.isImport(path)
        lazy val text = completionPos.cursorPos.source.content.mkString
        lazy val reverseTokens: Array[Token] =
          // Try not to tokenize the whole file
          // Maybe we should re-use the tokenize result with `notInComment`
          val lineStart =
            if completionPos.cursorPos.line > 0 then
              completionPos.sourcePos.source.lineToOffset(
                completionPos.cursorPos.line - 1
              )
            else 0
          text
            .substring(lineStart, completionPos.cursorPos.start)
            .tokenize
            .toOption match
            case Some(toks) => toks.tokens.reverse
            case None => Array.empty[Token]
        end reverseTokens

        val canBeExtended = KeywordCompletionsUtils.canBeExtended(reverseTokens)
        val canDerive = KeywordCompletionsUtils.canDerive(reverseTokens)
        val hasExtend = KeywordCompletionsUtils.hasExtend(reverseTokens)

        Keyword.all.collect {
          case kw
              if kw.matchesPosition(
                completionPos.query,
                isExpression = isExpression,
                isBlock = isBlock,
                isDefinition = isDefinition,
                isMethodBody = isMethodBody,
                isTemplate = isTemplate,
                isPackage = isPackage,
                isParam = isParam,
                isScala3 = true,
                isSelect = isSelect,
                isImport = isImport,
                allowToplevel = true,
                canBeExtended = canBeExtended,
                canDerive = canDerive,
                hasExtend = hasExtend,
              ) && notInComment =>
            CompletionValue.keyword(kw.name, kw.insertText)
        }
    end match
  end contribute

  private def checkIfNotInComment(
      pos: SourcePosition,
      path: List[Tree],
  ): Boolean =
    val text = pos.source.content
    val (treeStart, treeEnd) = path.headOption
      .map(t => (t.span.start, t.span.end))
      .getOrElse((0, text.size))
    val offset = pos.start
    text.mkString.checkIfNotInComment(treeStart, treeEnd, offset)
  end checkIfNotInComment

  private def isPackage(enclosing: List[Tree]): Boolean =
    enclosing match
      case Nil => true
      case _ => false

  private def isParam(enclosing: List[Tree]): Boolean =
    enclosing match
      case (vd: ValDef) :: (dd: DefDef) :: _
          if dd.paramss.exists(pc => pc.contains(vd) && pc.size == 1) =>
        true
      case _ => false

  private def isTemplate(enclosing: List[Tree]): Boolean =
    enclosing match
      case Ident(_) :: (_: Template) :: _ => true
      case Ident(_) :: (_: ValOrDefDef) :: _ => true
      case (_: TypeDef) :: _ => true
      case _ => false

  private def isMethodBody(enclosing: List[Tree]): Boolean =
    enclosing match
      case Ident(_) :: (_: DefDef) :: _ => true
      case _ => false

  private def isSelect(enclosing: List[Tree]): Boolean =
    enclosing match
      case (_: Apply) :: (_: Select) :: _ => true
      case (_: Select) :: _ => true
      case _ => false

  private def isImport(enclosing: List[Tree]): Boolean =
    enclosing match
      case Import(_, _) :: _ => true
      case _ => false

  private def isDefinition(
      enclosing: List[Tree],
      name: String,
      pos: SourcePosition,
  )(using ctx: Context): Boolean =
    enclosing match
      case (_: Ident) :: _ => false
      case _ =>
        // NOTE(olafur) in positions like "implicit obje@@" the parser discards the entire
        // statement and `enclosing` is not helpful. In these situations we fallback to the
        // diagnostics reported by the parser to see if it expected a definition here.
        // This is admittedly not a great solution, but it's the best I can think of at this point.
        val point = pos.withSpan(pos.span.withPoint(pos.point - name.length()))

        val isExpectedStartOfDefinition =
          ctx.reporter.allErrors.exists { info =>
            info.pos.focus == point &&
            info.message == "expected start of definition"
          }
        isExpectedStartOfDefinition

  private def isBlock(enclosing: List[Tree]): Boolean =
    enclosing match
      case Ident(_) :: Block(_, _) :: _ => true
      case _ => false

  private def isExpression(enclosing: List[Tree]): Boolean =
    enclosing match
      case Ident(_) :: (_: Template) :: _ => true
      case Ident(_) :: (_: ValOrDefDef) :: _ => true
      case Ident(_) :: t :: _ if t.isTerm => true
      case other => false
end KeywordsCompletions
