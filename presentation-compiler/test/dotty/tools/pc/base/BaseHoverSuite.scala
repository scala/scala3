package dotty.tools.pc.base

import java.nio.file.Paths

import scala.language.unsafeNulls
import scala.meta.internal.metals.{CompilerOffsetParams, CompilerRangeParams}

import dotty.tools.pc.utils.{RangeReplace, TestHovers}
import dotty.tools.pc.utils.InteractiveEnrichments.*

abstract class BaseHoverSuite
    extends BasePCSuite
    with TestHovers
    with RangeReplace:

  def check(
      original: String,
      expected: String,
      includeRange: Boolean = false
  ): Unit =
    val filename = "Hover.scala"
    val codeOriginal = original
      .replace("<<", "")
      .replace(">>", "")
    val (code, so, eo) = hoverParams(codeOriginal, filename)
    val pcParams = if so == eo then
      CompilerOffsetParams(Paths.get(filename).toUri(), code, so)
    else
      CompilerRangeParams(Paths.get(filename).toUri(), code, so, eo)
    val hover = presentationCompiler
      .hover(pcParams)
      .get()
      .asScala
      .map(_.toLsp())
    val obtained: String = renderAsString(code, hover, includeRange)

    assertNoDiff(expected, obtained)

    for
      h     <- hover
      range <- Option(h.getRange)
    do
      val base =
        codeOriginal.replace("@@", "").replace("%<%", "").replace("%>%", "")
      val withRange = replaceInRange(base, range)
      val expected = original
        .replace("@@", "")
        .replace("%<%", "")
        .replace("%>%", "")

      assertNoDiff(expected, withRange)
