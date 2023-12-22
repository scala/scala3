package dotty.tools.pc

import dotty.tools.dotc.ast.tpd.*
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
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.pc.utils.MtagsEnrichments.*
import org.eclipse.lsp4j as l

import scala.jdk.CollectionConverters.*
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

        val pos = driver.sourcePosition(params, isZeroExtent = false)
        val path = Interactive.pathTo(unit.tpdTree, pos.span)
        val (paramN, callableN, alternatives) = Signatures.signatureHelp(path, pos.span)

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
