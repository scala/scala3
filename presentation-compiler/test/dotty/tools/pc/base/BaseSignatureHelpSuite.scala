package dotty.tools.pc.base

import java.nio.file.Paths

import scala.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams
import scala.language.unsafeNulls

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
    if (result != null) {
      result.getSignatures.asScala.zipWithIndex.foreach { case (signature, i) =>
        if (includeDocs) {
          val sdoc = doc(signature.getDocumentation)
          if (sdoc.nonEmpty) {
            out.append(sdoc).append("\n")
          }
        }
        out
          .append(signature.getLabel)
          .append("\n")
        if (
          result.getActiveSignature == i && result.getActiveParameter != null && signature.getParameters
            .size() > 0
        ) {
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
            if (labels == 0) ch
            else seekColumn(ch + 1, labels - 1)
          val column = seekColumn(0, sameLabelsBeforeActive)
          if (column < 0) {
            fail(s"""invalid parameter label
                    |  param.label    : ${param.getLabel}
                    |  signature.label: ${signature.getLabel}
                    |""".stripMargin)
          }
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
            if (includeDocs && pdoc.nonEmpty) {
              out
                .append("  @param ")
                .append(param.getLabel.getLeft().replaceFirst("[ :].*", ""))
                .append(" ")
                .append(pdoc)
                .append("\n")
            }
          }
        }
      }
    }

    val obtainedSorted = sortLines(stableOrder, out.toString())
    val expectedSorted = sortLines(stableOrder, expected)
    assertCompletions(expectedSorted, obtainedSorted, Some(original))
