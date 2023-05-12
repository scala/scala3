package dotty.tools.pc.base

import dotty.tools.pc.utils.TestSemanticTokens

import java.net.URI
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerVirtualFileParams

class BaseSemanticTokensSuite extends BasePCSuite:

  // We check only if correct symbol tokens are added here.
  // Other tokens (e.g. keywords) are added outside the compiler.
  def check(
      expected: String
  ): Unit =
    val base =
      expected
        .replaceAll(raw"/\*[\w,]+\*/", "")
        .replaceAll(raw"\<\<|\>\>", "")
    val nodes = presentationCompiler
      .semanticTokens(
        CompilerVirtualFileParams(URI.create("file:/Tokens.scala"), base)
      )
      .get()

    val obtained = TestSemanticTokens.pcSemanticString(
      base,
      nodes.asScala.toList
    )
    assertNoDiff(expected, obtained)
