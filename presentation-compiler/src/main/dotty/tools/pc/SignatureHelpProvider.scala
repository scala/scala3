package dotty.tools.pc

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.parsing.Tokens.closingRegionTokens
import dotty.tools.dotc.reporting.ErrorMessageID
import dotty.tools.dotc.reporting.ExpectedTokenButFound
import dotty.tools.dotc.util.Signatures
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans
import dotty.tools.pc.utils.MtagsEnrichments.*
import org.eclipse.lsp4j as l

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.*
import scala.meta.pc.OffsetParams
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.SymbolSearch

object SignatureHelpProvider:

  def signatureHelp(
      driver: InteractiveDriver,
      params: OffsetParams,
      search: SymbolSearch
  ) =
    val uri = params.uri().nn
    val text = params.text().nn
    val sourceFile = SourceFile.virtual(uri, text)
    driver.run(uri.nn, sourceFile)

    driver.compilationUnits.get(uri) match
      case Some(unit) =>
        given newCtx: Context = driver.currentCtx.fresh.setCompilationUnit(unit)

        val pos = driver.sourcePosition(params)
        val treeSpanEnd = ctx.compilationUnit.tpdTree.span.end

        lazy val firstUnclosedErrorAfterPos =
          val unclosedErrors = ctx.reporter.allErrors.filter { _.msg match
            case err: ExpectedTokenButFound => closingRegionTokens.contains(err.expected)
            case _ => false
          }
          unclosedErrors.find(_.pos.span.start >= pos.span.end).flatMap(_.position.toScala)

        // If we try to run signature help for named arg which happens to be the last line of the code
        // it will return span outside tree, as parser ignores additional whitespaces:
        //   def foo(aaa: Int, bbb: Int) = ???
        //   foo(aaa // fail before writing the = sign
        val adjustedSpan = if pos.span.end > treeSpanEnd && firstUnclosedErrorAfterPos.nonEmpty then Spans.Span(treeSpanEnd)
          else pos.span


        val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, adjustedSpan)

        import dotty.tools.dotc.ast.tpd._

        val maybeNewSpan = firstUnclosedErrorAfterPos match
          case Some(errorPosition) =>
            path
              .headOption
              // if in the head of the path, there is a tree defined between cursor and error position we are in correct tree
              .filter(!_.existsSubTree(tree => tree.span.exists && tree.span.start >= pos.span.end))
              .toList
              // find possible trees which can be part of unclosed signature help
              .flatMap(_.filterSubTrees:
                case tree: (Apply | UnApply | AppliedTypeTree | TypeApply) => tree.span.end <= pos.span.end
                case _ => false
              ).filter(tree => tree.span.end <= errorPosition.end )
              .maxByOption(_.span.end)
              .map(tree => Spans.Span(tree.span.end)
            )

          case None => None


        val adjustedPath = maybeNewSpan match
          case Some(pos) => Interactive.pathTo(ctx.compilationUnit.tpdTree, pos)
          case _ => path

        val (paramN, callableN, alternatives) = Signatures.signatureHelp(adjustedPath, pos.span)

        val infos = alternatives.flatMap: signature =>
          signature.denot.map(signature -> _)

        val signatureInfos = infos.map { case (signature, denot) =>
          search.symbolDocumentation(denot.symbol) match
            case Some(doc) =>
              withDocumentation(
                doc,
                signature,
                denot.symbol.is(Flags.JavaDefined)
              ).getOrElse(signature)
            case _ => signature

        }

        new l.SignatureHelp(
          signatureInfos.map(signatureToSignatureInformation).asJava,
          callableN,
          paramN
        )
      case _ => new l.SignatureHelp()
  end signatureHelp

  private def withDocumentation(
      info: SymbolDocumentation,
      signature: Signatures.Signature,
      isJavaSymbol: Boolean
  ): Option[Signatures.Signature] =
    val allParams = info.parameters().nn.asScala
    def updateParams(
        params: List[Signatures.Param],
        index: Int
    ): List[Signatures.Param] =
      params match
        case Nil => Nil
        case head :: tail =>
          val rest = updateParams(tail, index + 1)
          allParams.lift(index) match
            case Some(paramDoc) =>
              val newName =
                if isJavaSymbol && head.name.startsWith("x$") then
                  paramDoc.nn.displayName()
                else head.name
              head.copy(
                doc = Some(paramDoc.docstring.nn),
                name = newName.nn
              ) :: rest
            case _ => head :: rest

    def updateParamss(
        params: List[List[Signatures.Param]],
        index: Int
    ): List[List[Signatures.Param]] =
      params match
        case Nil => Nil
        case head :: tail =>
          val updated = updateParams(head, index)
          updated :: updateParamss(tail, index + head.size)
    val updatedParams = updateParamss(signature.paramss, 0)
    Some(signature.copy(doc = Some(info.docstring().nn), paramss = updatedParams))
  end withDocumentation

  private def signatureToSignatureInformation(
      signature: Signatures.Signature
  ): l.SignatureInformation =
    val tparams = signature.tparams.map(Signatures.Param("", _))
    val paramInfoss =
      (tparams ::: signature.paramss.flatten).map(paramToParameterInformation)
    val paramLists =
      if signature.paramss.forall(_.isEmpty) && tparams.nonEmpty then ""
      else
        signature.paramss
          .map { paramList =>
            val labels = paramList.map(_.show)
            val prefix = if paramList.exists(_.isImplicit) then "using " else ""
            labels.mkString(prefix, ", ", "")
          }
          .mkString("(", ")(", ")")
    val tparamsLabel =
      if signature.tparams.isEmpty then ""
      else signature.tparams.mkString("[", ", ", "]")
    val returnTypeLabel = signature.returnType.map(t => s": $t").getOrElse("")
    val label = s"${signature.name}$tparamsLabel$paramLists$returnTypeLabel"
    val documentation = signature.doc.map(markupContent)
    val sig = new l.SignatureInformation(label)
    sig.setParameters(paramInfoss.asJava)
    documentation.foreach(sig.setDocumentation(_))
    sig
  end signatureToSignatureInformation

  /**
   * Convert `param` to `ParameterInformation`
   */
  private def paramToParameterInformation(
      param: Signatures.Param
  ): l.ParameterInformation =
    val documentation = param.doc.map(markupContent)
    val info = new l.ParameterInformation(param.show)
    documentation.foreach(info.setDocumentation(_))
    info

  private def markupContent(content: String): l.MarkupContent | Null =
    if content.isEmpty() then null
    else
      val markup = new l.MarkupContent
      markup.setKind("markdown")
      markup.setValue(content.trim())
      markup

end SignatureHelpProvider
