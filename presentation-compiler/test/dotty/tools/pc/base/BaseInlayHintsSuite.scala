package dotty.tools.pc.base

import java.net.URI

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerInlayHintsParams
import scala.meta.internal.metals.CompilerRangeParams
import scala.language.unsafeNulls

import dotty.tools.pc.utils.TestInlayHints
import dotty.tools.pc.utils.TextEdits

import org.eclipse.lsp4j.TextEdit

class BaseInlayHintsSuite extends BasePCSuite {

  def check(
      base: String,
      expected: String,
      kind: Option[Int] = None,
      hintsInPatternMatch: Boolean = false
  ): Unit =
    def pkgWrap(text: String) =
      if (text.contains("package")) text
      else s"package test\n$text"

    val withPkg = pkgWrap(base)
    val rangeParams = CompilerRangeParams(
      URI.create("file:/InlayHints.scala"),
      withPkg,
      0,
      withPkg.length()
    )
    val pcParams = CompilerInlayHintsParams(
      rangeParams,
      true,
      true,
      true,
      true,
      hintsInPatternMatch
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
      obtained,
    )

}