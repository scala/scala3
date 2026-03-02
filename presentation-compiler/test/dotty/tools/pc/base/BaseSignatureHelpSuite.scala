package dotty.tools.pc.base

import java.nio.file.Paths

import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import scala.meta.internal.metals.CompilerOffsetParams

import org.eclipse.lsp4j.SignatureHelp

abstract class BaseSignatureHelpSuite extends BasePCSuite:
  def checkDoc(
      code: String,
      expected: String
  ): Unit =
    check(code, expected, includeDocs = true)

  def check(
      original: String,
      expected: String,
      includeDocs: Boolean = false,
      stableOrder: Boolean = true
  ): Unit =
    val (code, offset) = params(original)
    val result =
      presentationCompiler
        .signatureHelp(
          CompilerOffsetParams(Paths.get("A.scala").toUri(), code, offset)
        )
        .get()
    val out = new StringBuilder()

    // this is default SignatureHelp value which should only be returned on crash
    assert(result != new SignatureHelp())

    if result != null then
      result.getSignatures.asScala.zipWithIndex.foreach { case (signature, i) =>
        if includeDocs then
          val sdoc = doc(signature.getDocumentation)
          if sdoc.nonEmpty then
            out.append(sdoc).append("\n")
        out
          .append(signature.getLabel)
          .append("\n")
        if result.getActiveSignature == i && result.getActiveParameter != null && result.getActiveParameter() >= 0 && result.getActiveParameter() < signature.getParameters.size()
        then
          val param = signature.getParameters.get(result.getActiveParameter)
          val label = param.getLabel.getLeft()
          /* We need to find the label of the active parameter and show ^ at that spot
               if we have multiple same labels we need to find the exact one.
           */
          val sameLabelsBeforeActive = signature.getParameters.asScala
            .take(result.getActiveParameter + 1)
            .count(_.getLabel().getLeft() == label) - 1
          def seekColumn(atIndex: Int, labels: Int): Int =
            val ch = signature.getLabel.indexOf(label, atIndex)
            if labels == 0 then ch
            else seekColumn(ch + 1, labels - 1)
          val column = seekColumn(0, sameLabelsBeforeActive)
          if column < 0 then
            fail(s"""invalid parameter label
                    |  param.label    : ${param.getLabel}
                    |  signature.label: ${signature.getLabel}
                    |""".stripMargin)
          val indent = " " * column
          out
            .append(indent)
            .append("^" * param.getLabel.getLeft().length)
            .append("\n")
          signature.getParameters.asScala.foreach { param =>
            val pdoc = doc(param.getDocumentation)
              .stripPrefix("```scala\n")
              .stripSuffix("\n```")
              .replace("\n```\n", " ")
            if includeDocs && pdoc.nonEmpty then
              out
                .append("  @param ")
                .append(param.getLabel.getLeft().replaceFirst("[ :].*", ""))
                .append(" ")
                .append(pdoc)
                .append("\n")
          }
      }

    val (obtainedSorted, _) = sortLines(stableOrder, out.toString())
    val (expectedSorted, _) = sortLines(stableOrder, expected)
    assertWithDiff(expectedSorted, obtainedSorted, includeSources = false, Some(original))
