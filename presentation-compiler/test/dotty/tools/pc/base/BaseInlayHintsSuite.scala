package dotty.tools.pc.base

import java.net.URI

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerInlayHintsParams
import scala.meta.internal.metals.CompilerRangeParams

import dotty.tools.pc.utils.TestInlayHints

class BaseInlayHintsSuite extends BasePCSuite:

  def check(
      base: String,
      expected: String,
      kind: Option[Int] = None,
      hintsInPatternMatch: Boolean = false,
      closingLabels: Boolean = false
  ): Unit =
    def pkgWrap(text: String) =
      if text.contains("package") then text
      else s"package test\n$text"

    val withPkg = pkgWrap(base)
    val rangeParams = CompilerRangeParams(
      URI.create("file:/InlayHints.scala"),
      withPkg,
      0,
      withPkg.length()
    )
    val pcParams = CompilerInlayHintsParams(
      rangeParams = rangeParams,
      inferredTypes = true,
      typeParameters = true,
      implicitParameters = true,
      hintsXRayMode = true,
      byNameParameters = true,
      implicitConversions = true,
      namedParameters = true,
      hintsInPatternMatch = hintsInPatternMatch,
      closingLabels = closingLabels
    )

    val inlayHints = presentationCompiler
      .inlayHints(
        pcParams
      )
      .get()
      .asScala
      .toList

    val obtained = TestInlayHints.applyInlayHints(withPkg, inlayHints)

    assertNoDiff(
      pkgWrap(expected),
      obtained
    )
